(load "/home/rick/Documents/University/lisp-oop-ui/src/ltk/ltk.lisp")
(in-package :ltk)


; -----------------------------------------------------------------------
(defclass BaseVoucher ()
	(
		(name :accessor voucher-name :initarg :name)
		(transport :accessor voucher-transport :initarg :transport)
		(food :accessor voucher-food :initarg :food)
		(duration :accessor voucher-duration :initarg :duration)
		(choosen :accessor is-choosen :initarg :choosen :initform nil)
	)
)

(defmethod print-voucher ((object BaseVoucher))
	(terpri)
	(format t "Voucher's name: ~S~%" (voucher-name object))
	(format t "Transport: ~S~%" (voucher-transport object))
	(format t "Food type: ~S~%" (voucher-food object))
	(format t "Voucher's duration: ~S~%" (voucher-duration object))
	(format t "Voucher's choosen: ~S~%" (is-choosen object))
)

(defmethod change-voucher ((voucher BaseVoucher) label errors transport food duration)
	(setf (text errors) " ")
	(cond 
		((eq (is-choosen voucher) t)
			(progn
				(setf (text label) (concatenate 'string "☆ " (voucher-name voucher)))
				(setf (is-choosen voucher) nil)
			)
		)
		(t
			(cond 
				(
					(or (string= transport "plane") (string= transport "bus") (string= transport "ship") (string= transport "train"))
						(setf (voucher-transport voucher) transport)
				)
				(t 
					(setf (text errors) (concatenate 'string (text errors) (concatenate 'string "  Invalid input in field transport in " (voucher-name voucher))))
				)
			)
			(cond 
				(
					(or (string= food "AI") (string= food "AP") (string= food "BB") (string= food "FA"))
						(setf (voucher-food voucher) food)
				)
				(t 
					(setf (text errors) (concatenate 'string (text errors) (concatenate 'string "  Invalid input in field food in " (voucher-name voucher))))
				)
			)
			(cond 
				(
					(and (> duration 0) (< duration 30))
						(setf (voucher-duration voucher) duration)
				)
				(t 
					(setf (text errors) (concatenate 'string (text errors) (concatenate 'string "  Invalid input in field duration in " (voucher-name voucher))))
				)
			)
			(if (string= (text errors) " ")
				(progn 
					(setf (text label) (concatenate 'string "★ " (voucher-name voucher)))
					(setf (is-choosen voucher) t)
					(print-voucher voucher)
				)
			)
		)
	)
)


; -----------------------------------------------------------------------
(defclass CruiseVoucher (BaseVoucher)
	(
		(route :accessor cruise-route :initarg :route)
	)
)

(defmethod print-voucher ((object CruiseVoucher))
	(call-next-method)
	(format t "Cruise's route: ~S~%" (cruise-route object))
)


; -----------------------------------------------------------------------
(defclass ExcursionVoucher (BaseVoucher)
	(
		(place :accessor excursion-place :initarg :place)
	)
)

(defmethod print-voucher ((object ExcursionVoucher))
	(call-next-method)
	(format t "Excursion's city: ~S~%" (excursion-place object))
)


; -----------------------------------------------------------------------
(defclass HolidayTrip ()
	(
		(vouchers-list :accessor holiday-vouchers
					   :initarg :vouchers-list
					   :initform (make-array 5 :fill-pointer 0 :adjustable t)
		)
	)
)

(defmethod print-trip-list (trip-list)
	(terpri)
	(loop for i in trip-list
		do (print-voucher i)
		(terpri)
	)
)

(defmethod sort-trip-by-duration ((trip HolidayTrip))
	(setq trip-list (map 'list #'identity (holiday-vouchers trip)))

	(print-trip-list trip-list)
	(format t "Sorted vouchers by duration: ")
	(sort trip-list #'< :key #'voucher-duration)
	(print-trip-list trip-list)
)


(defmethod print-trip ((trip HolidayTrip))
	(terpri)
	(setq trip-list (holiday-vouchers trip))
	(loop for i from 0 to (- (length trip-list) 1)
		do (print-voucher (aref trip-list i))
		(terpri)
	)
)

(defmethod choose-vouchers (vouchers)
	(setf mytrip (make-instance 'HolidayTrip))
	(print (length vouchers))
	(loop for i from 0 to (- (length vouchers) 1)
		do
		(if (eq (is-choosen (aref vouchers i)) t)
			(vector-push (aref vouchers i) (holiday-vouchers mytrip))
		)
	)
	(sort-trip-by-duration mytrip)
)


; -----------------------------------------------------------------------
(with-ltk ()
	(setf vouchers (make-array 3))
	(setf (aref vouchers 0) (make-instance 'CruiseVoucher
										:name "Pacific Cruise"
										:transport "plane"
										:food "AI"
										:duration 10
										:route "London-Miami"))

	(setf (aref vouchers 1) (make-instance 'ExcursionVoucher
										:name "Amsterdam sightseeing"
										:transport "bus"
										:food "BB"
										:duration 2
										:place "Amsterdam"))

	(setf (aref vouchers 2) (make-instance 'BaseVoucher
									:name "Amsterdam Holiday Inn"
									:transport "plane"
									:food "BB"
									:duration 3))

	(let* (
			(frame_ (make-instance 'frame))
			(text-label-errors (make-instance 'label :master frame_ :text "--------------" :font "sans 8"))
    		(text-label-voucher0 (make-instance 'label :master frame_ :text (concatenate 'string "☆ " (voucher-name (aref vouchers 0))) :font "sans 11"))
			(text-route-voucher0 (make-instance 'label :master frame_ :text (concatenate 'string "Cruise route: " (cruise-route (aref vouchers 0))) :font "sans 9"))
			(entry-transport-v0 (make-instance 'entry :master frame_ :text (voucher-transport (aref vouchers 0))))
			(entry-food-v0 (make-instance 'entry :master frame_ :text (voucher-food (aref vouchers 0))))
			(entry-duration-v0 (make-instance 'entry :master frame_ :text (voucher-duration (aref vouchers 0))))

			(btn-voucher0   (make-instance 'button :master frame_ :text "Voucher1"
                    			:command (lambda() 
                    				(change-voucher (aref vouchers 0) text-label-voucher0 text-label-errors (text entry-transport-v0) (text entry-food-v0) (parse-integer (text entry-duration-v0)))
                    				)
                    		)
			)

			(text-label-voucher1 (make-instance 'label :master frame_ :text (concatenate 'string "☆ " (voucher-name (aref vouchers 1))) :font "sans 11"))
			(text-place-voucher1 (make-instance 'label :master frame_ :text (concatenate 'string "Excursion city: " (excursion-place (aref vouchers 1))) :font "sans 9"))
			(entry-transport-v1 (make-instance 'entry :master frame_ :text (voucher-transport (aref vouchers 1))))
			(entry-food-v1 (make-instance 'entry :master frame_ :text (voucher-food (aref vouchers 1))))
			(entry-duration-v1 (make-instance 'entry :master frame_ :text (voucher-duration (aref vouchers 1))))

			(btn-voucher1   (make-instance 'button :master frame_ :text "Voucher2"
                    			:command (lambda() 
                    				(change-voucher (aref vouchers 1) text-label-voucher1 text-label-errors (text entry-transport-v1) (text entry-food-v1) (parse-integer (text entry-duration-v1)))
                    				)
                    		)
			)

			(text-label-voucher2 (make-instance 'label :master frame_ :text (concatenate 'string "☆ " (voucher-name (aref vouchers 2))) :font "sans 11"))
			(entry-transport-v2 (make-instance 'entry :master frame_ :text (voucher-transport (aref vouchers 2))))
			(entry-food-v2 (make-instance 'entry :master frame_ :text (voucher-food (aref vouchers 2))))
			(entry-duration-v2 (make-instance 'entry :master frame_ :text (voucher-duration (aref vouchers 2))))

			(btn-voucher2   (make-instance 'button :master frame_ :text "Voucher3"
                    			:command (lambda() 
                    				(change-voucher (aref vouchers 2) text-label-voucher2 text-label-errors (text entry-transport-v2) (text entry-food-v2) (parse-integer (text entry-duration-v2)))
                    				)
                    		)
			)

			(btn-sort   	(make-instance 'button :master frame_ :text "Sort"
                    			:command (lambda() 
                    				(choose-vouchers vouchers)
                    				)
                    		)
			)

		  )

		(pack frame_ :padx 20 :pady 20)
		(pack text-label-errors :padx 20 :pady 3)
		(pack text-label-voucher0 :padx 20 :pady 3)
		(pack entry-transport-v0 :padx 20 :pady 3)
		(pack entry-food-v0 :padx 20 :pady 3)
		(pack entry-duration-v0 :padx 20 :pady 3)
		(pack text-route-voucher0 :padx 20 :pady 3)
		(pack btn-voucher0 :padx 20 :pady 3)

		(pack text-label-voucher1 :padx 20 :pady 10)
		(pack entry-transport-v1 :padx 20 :pady 3)
		(pack entry-food-v1 :padx 20 :pady 3)
		(pack entry-duration-v1 :padx 20 :pady 3)
		(pack text-place-voucher1 :padx 20 :pady 3)
		(pack btn-voucher1 :padx 20 :pady 3)

		(pack text-label-voucher2 :padx 20 :pady 10)
		(pack entry-transport-v2 :padx 20 :pady 3)
		(pack entry-food-v2 :padx 20 :pady 3)
		(pack entry-duration-v2 :padx 20 :pady 3)
		(pack btn-voucher2 :padx 20 :pady 3)

		(pack btn-sort :padx 20 :pady 3)

		(configure frame_ :borderwidth 1)
	)
)
