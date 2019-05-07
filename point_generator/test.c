#include <sns.h>
#include <syslog.h>

int main( ) {
    sns_init();
    /*-- Open channel -- */
    ach_channel_t chan;
    sns_chan_open( &chan, "points", NULL );
    {
        ach_channel_t *chans[] = {&chan, NULL};
        sns_sigcancel( chans, sns_sig_term_default );
    }

    /*-- Construct Message --*/

    struct sns_msg_vector *msg = sns_msg_vector_heap_alloc(8);

    for(size_t i=0; i < 8; i++){
      msg->x[i] = 1;
    }

    struct timespec now;
    clock_gettime( ACH_DEFAULT_CLOCK, &now );
    sns_msg_set_time( &msg->header, &now, 0 ); /* 1 second duration */


    /*-- Send Message --*/
    if( SNS_LOG_PRIORITY(LOG_INFO) ) sns_msg_vector_dump( stdout, msg );
    ach_status_t r = sns_msg_vector_put(&chan, msg);
    sns_end();
    if( ACH_OK == r ) return 0;
    return 1;
}
