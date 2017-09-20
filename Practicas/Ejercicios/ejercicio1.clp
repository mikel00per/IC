;______________________________________________________________________________;
;
;  ejercicio1.clp | Menú para elegir una opción
;  Antonio Miguel Morillo Chica
;
;  Crea un trozo de código en CLIPS (cuantas menos reglas se usen mejor)
;  que muestre al usuario un conjunto de opciones y recoja la elección del
;  usuario añadiendo el hecho (OpcionElegida XXX) siendo XXX la etiqueta
;  utilizada para la opción elegida. Debe detectar errores en la entrada y
;  entonces volver a solicitar la elección.
;
;______________________________________________________________________________;

(defrule inicio
  =>
  (printout t "Elige una de las opciones siguientes." crlf )
  (assert (inicio-realizado))
)

(defrule elegirOpcion
  ?f <- (inicio-realizado)
  =>
  (printout t "    1: Opcion 1" crlf )
  (printout t "    2: Opcion 2" crlf )
  (printout t "    3: Opcion 3" crlf)
  (printout t " -> ")
  (bind ?opcion (read))
  (assert (opcion-elegida ?opcion))
  (retract ?f)
)

(defrule mostrarOpcion
  (opcion-elegida ?eleccion)
  =>
  (if (and (> ?eleccion 0) (< ?eleccion 4))
  then
  (printout t "Has elegidola opcion " ?eleccion crlf)
  else
  (printout t "Opcion incorrecta." crlf)
  (assert (inicio-realizado))
  )
)


; (deffacts elMenu
;  (elegir)
;)
;
;(defrule elegir
;  ?f <- (elegir)
;  =>
;  (printout t "1: Opcion1" crlf )
;  (printout t "2: Opcion2" crlf )
;  (printout t "3: Opcion3" crlf)
;
;  (bind ?opcion (read))
;  (assert (opcion-elegida ?opcion))
;  (retract ?f)
;)
;
;(defrule mostrarOpcion
;  (opcion-elegida ?eleccion)=>
;  (if (and (> ?eleccion 0) (< ?eleccion 4))
;  then
;  (printout t "Has elegido: " ?eleccion crlf)
;  else
;  (printout t "Opcion incorrecta." crlf)
;  (assert (elegir))
;  )
;)
