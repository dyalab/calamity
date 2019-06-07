(define (problem lamp-light)
	(:domain circuits)
	(:objects wire_a wire_b wire_c wire_d - wire
		 switch_a - switch
		 lamp_a - lamp
		 battery_a - battery
		 location_a location_b location_c location_d location_e location_f - location
		 location_wh location_sh location_lh - location)
	(:init (probabilistic 0.6 (at wire_a location_wh))
	       (at wire_b location_b)
	       (at wire_c location_d)
	       (at wire_d location_wh)
	       (probabilistic 0.9 (at switch_a location_c))
	       (at lamp_a location_e)
	       (at battery_a location_a)
	       (handempty))
	(:goal (and (at battery_a location_a)
		    (connected location_b)
	       	    (at switch_a location_c)
	       	    (connected location_d)
	       	    (at lamp_a location_e)
	       	    (connected location_f))))