(define (problem lamp-light)
	(:domain circuits)
	(:objects wire_a wire_b wire_c wire_d - wire
		 switch_a - switch
		 lamp_a - lamp
		 battery_a - battery
		 location_a location_b location_c location_d location_e location_f - location
		 location_wh location_sh location_lh - location)
	(:init
		(at battery_a location_a)
		(at wire_a location_wh)
		(at wire_b location_wh)
		(at wire_c location_wh)
		(at wire_d location_wh)
		(at switch_a location_sh)
		(at lamp_a location_lh))
	(:goal ((and (at battery_a location_a)
		     (connected-wire location_b)
	       	     (connected-switch location_c)
	       	     (connected-wire location_d)
	       	     (connected-lamp location_e)
	       	     (connected-wire location_f)))))