(define (domain circuits)
	(:requirements :typing)
	(:types	moveable - object
		switch - moveable
		wire - moveable
		lamp - moveable
		battery - object
		location)
	(:predicates (connected ?y - location)
		     (occupied ?y - location)
		     (at ?x - object ?y - location)
		     (holding ?x - moveable)
		     (handempty))
	(:action pick-up
		 :parameters (?x - moveable ?y - location)
		 :precondition (and (at ?x ?y)
		 	       	    (handempty))
		 :effect (and (holding ?x)
		 	      (not (at ?x ?y))
			      (not (connected ?y))
			      (not (handempty))
			      (not (occupied ?y))))
	(:action put-down
		 :parameters (?x - moveable ?y - location)
		 :precondition (and (holding ?x)
		 	       	    (not (occupied ?y )))
		 :effect (and (not (holding ?x))
		 	      (handempty)
			      (occupied ?y)
			      (connected ?y)
			      (at ?x ?y))))