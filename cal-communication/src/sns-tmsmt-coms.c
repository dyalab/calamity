/*
 * Copyright (c) 2019 Colorado School of Mines
 *
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 *     * Redistributions of source code must retain the above
 *       copyright notice, this list of conditions and the following
 *       disclaimer.
 *     * Redistributions in binary form must reproduce the above
 *       copyright notice, this list of conditions and the following
 *       disclaimer in the documentation and/or other materials
 *       provided with the distribution.
 *     * Neither the name of the copyright holder nor the names of its
 *       contributors may be used to endorse or promote products
 *       derived from this software without specific prior written
 *       permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
 * "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
 * LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
 * FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
 * COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT,
 * INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
 * (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
 * SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
 * HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT,
 * STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
 * ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED
 * OF THE POSSIBILITY OF SUCH DAMAGE.
 */

#include "config.h"

#include <getopt.h>
#include <sys/stat.h>
#include <unistd.h>

#include <amino.h>
#include <ach.h>
#include <tmplan.h>

#include <amino/rx/scenegraph.h>

#include <amino/ct/state.h>
#include <amino/ct/traj.h>

#include <sns.h>
#include <sns/motor.h>
#include <sns/event.h>
#include <ach/experimental.h>

struct cx {
    struct aa_rx_sg *scenegraph;

    struct sns_motor_channel *arm_in;
    struct sns_motor_channel *arm_out;

    struct sns_motor_ref_set *ref_set;
    struct sns_motor_state_set *state_set;

    aa_rx_frame_id end_effector;

    struct sns_evhandler *handlers;

    double t;
    double duration;
    int64_t timestep;
    struct timespec cur_time;

    char *plan_file;
    struct timespec file_mod_time;
    struct stat file_stat;

    struct ach_channel reparent;
    struct ach_channel action_chan;

    char *prev_action;

    struct aa_ct_state *state_ref;
    struct aa_ct_seg_list *seg_list;
    struct tmplan *plan;

    struct aa_mem_region* reg;
    struct aa_ct_limit* limit;

    int only_one;
};


static void halt( struct cx *cx );
static void reset_list( struct cx *cx);
static void send_action(struct cx *cx);
enum ach_status send_interp(void *cx_);
static void parse_operation(struct tmplan_op *op, struct cx *cx);

double opt_frequency = 100;
const double epsilon = 0.005;


int main(int argc, char **argv){
  struct cx cx = {0};
  char *opt_end_effector=NULL, *opt_input_file=NULL, *opt_change_chan=NULL,
    *opt_action_channel=NULL;


  int c = 0, opt_only_one=0;
  while( (c = getopt( argc, argv, "a:u:y:e:h?f:p:c:o" SNS_OPTSTRING)) != -1 ) {
    switch(c) {
      SNS_OPTCASES_VERSION("sns-amino-controller",
                           "Copyright (c) 2019, Colorado School of Mines\n",
                           "Matthew A. Schack")
    case 'a':
      opt_action_channel = strdup(optarg);
      break;
    case 'f':
      opt_frequency = atoi(optarg);
      break;
    case 'u':
      sns_motor_channel_push( optarg, &cx.arm_out );
      break;
    case 'y':
      sns_motor_channel_push( optarg, &cx.arm_in );
      break;
    case 'c':
      opt_change_chan = strdup(optarg);
      break;
    case 'p':
      opt_input_file = strdup(optarg);
      break;
    case 'e':
      opt_end_effector = optarg;
      break;
    case 'o':
      opt_only_one=1;
      break;
    case '?':   /* help     */
    case 'h':
      puts( "Usage: sns-amino-ctrl -u <from-arm> -y <to-arm>\n"
            "Teleop a robot.\n"
            "\n"
            "Options:\n"
            "  -y <channel>,             current state of the arm (state)\n"
            "  -u <channel>,             Motor velocities to move (ref)\n"
            "  -p <file>,                File name for an input plan\n"
            "  -e <frame>,               end-effector frame\n"
            "  -V,                       Print program version\n"
            "  -?,                       display this help and exit\n"
            "Report bugs to " PACKAGE_BUGREPORT );
      exit(EXIT_SUCCESS);
    default:
      SNS_DIE("Unknown Option: `%c'\n", c);
      break;
    }
  }

  sns_init();
  SNS_REQUIRE(cx.arm_in, "Need input channel from arm");
  SNS_REQUIRE(cx.arm_out, "Need output channel to arm");
  SNS_REQUIRE(opt_change_chan, "Need channel to broadcast reparents");
  SNS_REQUIRE(opt_action_channel, "Need channel to broadcast performed actions");
  SNS_REQUIRE(opt_input_file, "Need input file to read for points");

  /* Initalize memory for amino states */
  cx.reg = aa_mem_region_create(32768); //arbitrarly large number

  /* Load Scene Plugin */
  cx.scenegraph = sns_scene_load();
  aa_rx_sg_init(cx.scenegraph);

  if( opt_end_effector ) {
    cx.end_effector = aa_rx_sg_frame_id(cx.scenegraph,opt_end_effector);
    SNS_REQUIRE( cx.end_effector > 0, "Invalid end-effector frame: `%s'", opt_end_effector );
  } else {
    cx.end_effector = AA_RX_FRAME_NONE;
  }

  /* Output channel to arm */
  sns_motor_ref_init(cx.scenegraph,
                     cx.arm_out, &cx.ref_set,
                     0, NULL );


  /* Input channel from arm */
  size_t n_state = sns_motor_channel_count(cx.arm_in);
  size_t handler_count = n_state;
  cx.handlers = AA_NEW_AR(struct sns_evhandler, handler_count);
  sns_motor_state_init(cx.scenegraph,
                       cx.arm_in, &cx.state_set,
                       n_state,cx.handlers);


  /* Reparent channel */
  sns_chan_open(&cx.reparent, opt_change_chan, NULL);

  /* Action channel */
  sns_chan_open(&cx.action_chan, opt_action_channel, NULL);

  /* Make limit states */
  size_t n_q = cx.ref_set->n_q;
  double min_q[n_q];
  double min_dq[n_q];
  double min_ddq[n_q];
  double max_q[n_q];
  double max_dq[n_q];
  double max_ddq[n_q];

  for(size_t i=0; i< n_q; i++){
    min_q[i] = -M_PI/2;
    min_dq[i] = -0.25;
    min_ddq[i] = -0.05;
    max_q[i] = M_PI/2;
    max_dq[i] = 0.25;
    max_ddq[i] = 0.05;
  }

  struct aa_ct_state min;
  min.n_q = n_q;
  min.q = min_q;
  min.dq = min_dq;
  min.ddq = min_ddq;

  struct aa_ct_state max;
  max.n_q = n_q;
  max.q = max_q;
  max.dq = max_dq;
  max.ddq = max_ddq;

  struct aa_ct_limit limit;
  limit.min = &min;
  limit.max = &max;

  cx.limit = &limit;

  /* Prepare frequency calculations */
  cx.duration = (1 / opt_frequency);
  cx.timestep = (int64_t) (cx.duration *1e9);

  struct timespec sleep_dur;
  sleep_dur.tv_sec = cx.timestep / (int64_t) (1e9);
  sleep_dur.tv_nsec = cx.timestep % (int64_t) (1e9);

  struct timespec time;
  clock_gettime(ACH_DEFAULT_CLOCK, &time);
  cx.cur_time = time;
  cx.t=0;

  /* State allocation */
  cx.state_ref = aa_ct_state_alloc(cx.reg,n_q,n_q);


  /* Initalize ref set */
  for( size_t i = 0; i < cx.ref_set->n_q; i++ ) {
    cx.ref_set->u[i] = 0;
    cx.ref_set->meta[i].mode = SNS_MOTOR_MODE_VEL;
  }

  /* Initialize a plan from the file */

  struct stat attr;
  cx.file_stat = attr;
  cx.plan_file = opt_input_file;
  cx.prev_action = NULL;

  if( stat(opt_input_file, &attr) == 0 ){ // Success
    SNS_LOG(LOG_DEBUG, "Creating segment list from file: %s\n", opt_input_file);
    cx.file_mod_time = attr.st_mtim;
    cx.plan = tmplan_parse_filename(opt_input_file, cx.reg);
  }else{
    SNS_LOG(LOG_DEBUG, "Error opening file: %s\n", opt_input_file);
    cx.plan = NULL;
    struct timespec dummy_time;
    dummy_time.tv_sec = 0;
    cx.file_mod_time = dummy_time;
  }

  cx.only_one = opt_only_one;

  /* Loop through points and send the interpolation */
  enum ach_status r = sns_evhandle(cx.handlers,handler_count,
                                   &sleep_dur, send_interp, &cx,
                                   sns_sig_term_default,
                                   ACH_EV_O_PERIODIC_TIMEOUT);

  halt(&cx);
  SNS_REQUIRE( sns_cx.shutdown || (ACH_OK == r),
                 "Could not handle event: %s, %s\n",
                 ach_result_to_string(r),
                 strerror(errno) );

  sns_end();
  return 0;
}


enum ach_status send_interp(void *cx_){
  struct cx *cx = (struct cx*)cx_;


  /* Check to see if the plan file has been updated */
  if( stat(cx->plan_file, &cx->file_stat) == 0 ){ //Success
    if( cx->file_mod_time.tv_sec == 0 ||
        cx->file_mod_time.tv_sec < cx->file_stat.st_mtim.tv_sec ||
        (cx->file_mod_time.tv_sec == cx->file_stat.st_mtim.tv_sec &&
         cx->file_mod_time.tv_nsec < cx->file_stat.st_mtim.tv_nsec)){
      /* Stop the arm */
      reset_list(cx);

      /* Remake the plan */
      if(cx->plan) aa_mem_region_pop(cx->reg, cx->plan);
      cx->prev_action = NULL;
      cx->plan = tmplan_parse_filename(cx->plan_file, cx->reg);

      /* Update the saved mod time */
      cx->file_mod_time = cx->file_stat.st_mtim;
    }
  }

  /* If we don't know where to go, check to see if there are more steps to the plan */
  if( !cx->seg_list && cx->plan){
    SNS_LOG(LOG_DEBUG, "No seg list, trying to make a new one.\n");
    struct tmplan_op *op = tmplan_remove_first(cx->plan);
    parse_operation(op, cx);
  }

  /* Quit if we only want to move one plan instance and we've completed it */
  if (!cx->seg_list && cx->only_one) {sns_cx.shutdown=1; return ACH_CANCELED;}

  /* Don't do anything if we don't know where to go */
  if ( !cx->seg_list ) return ACH_OK;

  /* Update time */
  cx->t += cx->duration;
  SNS_LOG(LOG_DEBUG, "time: %f\n",cx->t);

  if(sns_cx.shutdown){
    return ACH_CANCELED;
  }


  /* interpolate */
  int r = aa_ct_seg_list_eval(cx->seg_list,cx->state_ref,cx->t);
  if (!r) SNS_LOG(LOG_DEBUG, "out of seg list bounds\n");

  double kp = 1; //TODO: make this not a magic number
  int h = 1; //check to see if we should just send the halt command.
  struct aa_ct_state* max = cx->limit->max;
  struct aa_ct_state* min = cx->limit->min;


  for(size_t i=0; i < cx->ref_set->n_q; i++){
    double state_q = cx->state_set->state->q[i];
    double ref_q = cx->state_ref->q[i];

    double max_dq = max->dq[i];
    double min_dq = min->dq[i];

    /* Calculate new motor velocity */
    double new_vel = kp*(ref_q - state_q);
    if (r) new_vel = new_vel + cx->state_ref->dq[i];

    if (new_vel > max_dq) new_vel = max_dq;
    if (new_vel < min_dq) new_vel = min_dq;

    if(fabs(state_q-ref_q) < epsilon) new_vel = 0;
    else h = 0;			/* Don't sent halt, we are moving at least 1 joint */
    cx->ref_set->u[i] = new_vel;

    SNS_LOG(LOG_DEBUG, "%lu reference q: %f. Actual q: %f. ref_dq: %f dq: %f\n", i, ref_q,
            state_q, cx->state_ref->dq[i], new_vel);
  }


  /* Increment time */
  cx->cur_time = sns_time_add_ns(cx->cur_time,cx->timestep);

  /* Send message */
  if (!r && h) { // halt and deallocate memory if we've reached the end
    reset_list(cx);
  } else  sns_motor_ref_put(cx->ref_set, &cx->cur_time, cx->timestep);
  return ACH_OK;
}

static void parse_operation(struct tmplan_op *op_, struct cx *cx){
  /* We have processed the last element destroy the plan */
  SNS_LOG(LOG_DEBUG, "parse operation\n");
  if( !op_){
    if(cx->prev_action) send_action(cx); // Consider the last action as performed
    aa_mem_region_pop(cx->reg, cx->plan);
    cx->plan = NULL;
    return;
  }
  SNS_LOG(LOG_DEBUG, "trying to figure out the next thing: %d\n", tmplan_op_type(op_));
  SNS_LOG(LOG_DEBUG, "ACTION: %d\n", TMPLAN_OP_ACTION);
  switch ( tmplan_op_type( op_ ) ){

    /* Create a new segment list from the motion plan */
  case TMPLAN_OP_MOTION_PLAN:;
    SNS_LOG(LOG_DEBUG, "parsing motion plan\n");
    struct tmplan_op_motion_plan *op = (struct tmplan_op_motion_plan*) op_;
    double* points = tmplan_op_motion_plan_path( op );
    size_t n_q = cx->ref_set->n_q;
    size_t size = tmplan_op_motion_plan_path_size( op );
    size_t n_points = size / n_q;

    if(n_points == 0) goto RECURSE;

    struct aa_ct_pt_list *pt_list = aa_ct_pt_list_create(cx->reg);
    aa_ct_pt_list_add_q(pt_list,n_q,cx->state_set->state->q);

    SNS_LOG(LOG_DEBUG, "Added point: ");
    for ( size_t i = 0; i < n_q; i++){
        SNS_LOG(LOG_DEBUG, "%f ", cx->state_set->state->q);
    }
    SNS_LOG(LOG_DEBUG, "\n");

    for ( size_t i = 0; i < n_points; i++){
      aa_ct_pt_list_add_q(pt_list, n_q, &points[i*n_q]);
      SNS_LOG(LOG_DEBUG, "Added point: ");
      for (size_t j=0; j < n_q; j++){
        SNS_LOG(LOG_DEBUG, "%f ", points[i*n_q+j]);
      }
      SNS_LOG(LOG_DEBUG, "\n");
    }

    if(aa_ct_pt_list_size(pt_list) <= 1) {aa_ct_pt_list_destroy(pt_list); goto RECURSE;}

    if(cx->seg_list) reset_list(cx);

    cx->seg_list = aa_ct_tjq_lin_generate(cx->reg, pt_list, cx->limit);
    aa_ct_pt_list_destroy(pt_list);

    clock_gettime(ACH_DEFAULT_CLOCK, &cx->cur_time);
    cx->t=0;
    return;

    /* Reparent frames that need to be reparented */
  case TMPLAN_OP_REPARENT:;
    SNS_LOG(LOG_DEBUG, "parsing reparent\n");
    struct tmplan_op_reparent *reparent_op = (struct tmplan_op_reparent*) op_;
    size_t n_frame = 0, n_parent = 0;
    const char *frame  = tmplan_op_reparent_get_frame( reparent_op);
    const char *parent = tmplan_op_reparent_get_new_parent(reparent_op);
    while( '\0' != frame[n_frame]){
      n_frame++;
    }
    while( '\0' != parent[n_parent]){
      n_parent++;
    }

    double q[4] = {0,0,0,1};
    double v[3] = {0,0,0};

    SNS_LOG(LOG_DEBUG, "frame: %s. Parent: %s\n", frame, parent);
    sg_make_reparent_and_send( cx->scenegraph, &cx->reparent,
                               aa_rx_sg_frame_id(cx->scenegraph, frame),
                               aa_rx_sg_frame_id(cx->scenegraph, parent),
                               q, v, cx->reg);

    goto RECURSE;

    /* If we have read an action, write the past action to a channel if it exists */
  case TMPLAN_OP_ACTION:;
    SNS_LOG(LOG_DEBUG, "parsing action\n");
    struct tmplan_op_action *action_op = (struct tmplan_op_action*) op_;
    char* new_action = tmplan_op_action_get(action_op);
    cx->prev_action = new_action;
    if(cx->prev_action) send_action(cx);
    //cx->prev_action = new_action;

    /* Recurse if there is more to the list, otherwise just return */
  default:
    SNS_LOG(LOG_DEBUG, "recursing\n");
    goto RECURSE;
  }

 RECURSE:;
  struct tmplan_op *new_op = tmplan_remove_first(cx->plan);
  parse_operation(new_op, cx);
  return;
}

static void send_action(struct cx *cx){
  SNS_LOG(LOG_DEBUG, "sending action\n");
  size_t a_size = 0;
  char* action = cx->prev_action;
  while( '\0' != action[a_size]) a_size++;
  struct sns_msg_text *msg = sns_msg_text_region_alloc(cx->reg, a_size);
  strcpy(msg->text, action);
  sns_msg_set_time(&msg->header, &cx->cur_time, 0 );
  sns_msg_text_put(&cx->action_chan, msg);
  cx->prev_action = NULL;
}


static void reset_list(struct cx *cx){
  halt(cx);
  if(cx->seg_list) aa_ct_seg_list_destroy(cx->seg_list);
  cx->seg_list = NULL;
}

static void
halt( struct cx *cx ){
    for( size_t i = 0; i < cx->ref_set->n_q; i++ ) {
        cx->ref_set->meta[i].mode = SNS_MOTOR_MODE_HALT;
        cx->ref_set->u[i]= 0;
    }

    struct timespec now;
    clock_gettime( ACH_DEFAULT_CLOCK, &now );
    sns_motor_ref_put( cx->ref_set, &now, 1e9 );

    for( size_t i = 0; i < cx->ref_set->n_q; i++ ) {
        cx->ref_set->meta[i].mode = SNS_MOTOR_MODE_VEL;
        cx->ref_set->u[i]= 0;
    }
}
