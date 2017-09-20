(deftemplate Licencia
  (field nombre)
  (field libre)
  (field os)
  (field copyleft)
  (field patente)
)

(deftemplate relacion
  (field Licencia)
  (field Licencia2)
  (multifield explicacion)
)

; --------------------------------------------------------------------
; Conocimiento de las compatibilidades:
;
;----------------------------------------------------------------------

(deffacts listaCaracteristicas
  (Licencia (nombre GPL)     (libre 3)  (os 3)  (copyleft 3) (patente 3))
  (Licencia (nombre LGPL)    (libre 3)  (os 2)  (copyleft 2) (patente 3))
  (Licencia (nombre MPL)     (libre 1)  (os 2)  (copyleft 0) (patente 0))
  (Licencia (nombre MIT/X11) (libre 3)  (os 2)  (copyleft 1) (patente 3))
  (Licencia (nombre RP)      (libre 0)  (os 1)  (copyleft 3) (patente 3))
)

(defrule inicio
  =>
  (printout t "Este es el sistema experto para licencias y proteccion de datos." crlf )
  (printout t "Que desea hacer?" crlf )
  (assert (inicio-realizado))
)

(defrule elegirOpcion
  ?m <- (inicio-realizado)
  =>
  (printout t "    1: Elegir licencia" crlf )
  (printout t "    2: Comparar licencia" crlf )
  (printout t "    3: Protección de datos" crlf)
  (printout t crlf)
  (printout t " -> ")
  (bind ?opcion (read))
  (assert (opcion-elegida ?opcion))
  (retract ?m)
)

(defrule mostrarOpcion
  (opcion-elegida ?eleccion)
  =>
  (if (and (> ?eleccion 0) (< ?eleccion 4)) then
    (if (= ?eleccion 1) then (assert (modulo-uno)))
    (if (= ?eleccion 2) then (assert (modulo-dos)))
    (if (= ?eleccion 2) then (assert (modulo-tres)))
  else
    (printout t "Opcion incorrecta." crlf)
    (assert (inicio-realizado))
  )
)

;--------------------------------------------------
;   MODULO 1:
;
;--------------------------------------------------


(defrule questionOne
  (modulo-uno)
  =>
  (printout t crlf)
  (printout t "MODULO 1" crlf)
  (printout t "En este modulo induciremos cual es la licencia que más se adapta a sus necesidades, según una serie de preguntas clave. No serán muchas :3" crlf)
  (printout t crlf)

  (printout t "¿Quiere derecho de patente? si/no" crlf)
  (bind ?respuesta (read))
  (if (eq ?respuesta si) then (printout t "Restantes: Republic Domain, MPL, LPGL y GLP")  (printout t crlf))
  (assert (respuesta-uno ?respuesta))
)

(defrule questionTwo
  (respuesta-uno ?r)
  =>
  (printout t crlf)
  (printout t "¿Quieres que sea propietario? (si/no)" crlf)
  (bind ?respuesta (read))
  (if (eq ?respuesta no) then (printout t "Restantes: MPL, LPGL y GLP") (printout t crlf))
  (assert (respuesta-dos ?respuesta))
)

(defrule questionThree
  (respuesta-dos ?r)
  =>
  (printout t crlf)
  (printout t "¿Quieres que se se use tu software y terceros añadan restricciones? (si/no)" crlf)
  (bind ?respuesta (read))
  (if (eq ?respuesta si) then (printout t "Restantes: MPL y LPGL")  (printout t crlf))
  (assert (respuesta-tres ?respuesta))
)

(defrule question-for
  (respuesta-tres ?r)
  =>
  (printout t crlf)
  (printout t "¿Quiere que se aplique la propiedad a nivel de archivo o libreria?" crlf)
  (bind ?respuesta (read))
  (assert (respuesta-cuatro ?respuesta))
)


(defrule arbol-decision
  (respuesta-uno ?r1)
  (respuesta-dos ?r2)
  (respuesta-tres ?r3)
  (respuesta-cuatro ?r4)
  =>
  (printout t crlf)
  (if (eq ?r1 no) then
    (if (eq ?r2 si) then
      (printout t "La mejor licencia es: Republic Domain" crlf)
      (printout t "Razón: Es una licencia permisiva por lo que puede ser libre o privada." crlf)
    else
      (if (eq ?r3 si) then
        (if (eq ?r4 archivo) then
          (printout t "La mejor licencia es: MPL" crlf)
          (printout t "Razón: Es una licencia de medium copyleft a nivel de archivos" crlf)
        else
          (printout t "La mejor licencia es: LPGL" crlf)
          (printout t "Razón: Es una licencia de low copyleft a nivel de librerias" crlf)
        )
      else
        (printout t "La mejor licencia es: GLP" crlf)
        (printout t "Razón: La licencia GLP es muy restrictiva con el copyleft por lo que si tu proyecto no cambirá sus restricciones si lo usan terceros." crlf)
      )
    )
  else
    (printout t "La mejor licencia es: MIT/X11" crlf)
    (printout t "Razón: Es la única licencia sin derechos de patente." crlf)
  )
)

;--------------------------------------------------
;   MODULO 2:
;
;--------------------------------------------------

(defrule licenciaOne
  (modulo-dos)
  =>
  (printout t crlf)
  (printout t "MODULO 2" crlf)
  (printout t "En este modulo induciremos la impativilidad de dos licencias" crlf)
  (printout t crlf)

  (printout t "LicenciaTipo I: " )
  (bind ?respuesta (read))
  (assert (respuesta-m21 ?respuesta))
)

(defrule licenciaTwo
  (respuesta-m21 ?l1)
  =>
  (printout t "LicenciaTipo II: " )
  (bind ?respuesta (read))
  (assert (respuesta-m22 ?respuesta))
  (assert (comparar-copyleft))
)

(defrule copyleft
  (comparar-copyleft)
  (respuesta-m21 ?l1)
  (respuesta-m22 ?l2)
  (Licencia (nombre ?l1) (copyleft ?cl1))
  (Licencia (nombre ?l2) (copyleft ?cl2))
  =>
  (if (<= ?cl1 ?cl2)
    then
      (assert (comparar-patente))
    else
      (printout t "Licencias incompatibles porque el copyleft de " ?l1 " es muy restrictivo" crlf)
  )
)

(defrule patente
  (comparar-patente)
  (respuesta-m21 ?l1)
  (respuesta-m22 ?l2)
  (Licencia (nombre ?l1) (patente ?p1))
  (Licencia (nombre ?l2) (patente ?p2))
  =>
  (if (and (= ?p1 0) (> ?p2 1))
    then
      (printout t "La licencia " ?l2 " no es compatible con la " ?l1 " porque " ?l1 " no tiene derechos de patente a diferencia de " ?l2 crlf)
    else
      (if (and (= ?p2 0) (> ?p1 1))
        then
          (printout t "La licencia " ?l1 " no es compatible con la " ?l2 " porque " ?l2 " no tiene derechos de patente a diferencia de " ?l1  crlf)
        else
          (printout t "La licencia " ?l2 " es compatible a nivel de patente " ?l1 crlf)
          (assert (comparar-os))
      )
  )
)

(defrule so
  (comparar-os)
  (respuesta-m21 ?l1)
  (respuesta-m22 ?l2)
  (Licencia (nombre ?l1) (os ?os1))
  (Licencia (nombre ?l2) (os ?os2))
  =>
  (if (and (= ?os1 1) (> ?os2 1))
    then
      (printout t "La licencia " ?l2 " no es compatible con " ?l1 )
      (printout t " debido a que " ?l1 " no es una licencia para soft propietario y " ?l2 " es para soft libre " crlf)
    else
      (if (and (> ?os1 1) (= ?os2 0))
        then
        (printout t "La licencia " ?l2 " no es compatible con " ?l1 )
        (printout t " debido a que " ?l1 " es para soft libre y " ?l2 " es una licencia para soft propietario  " crlf)
        else
          (assert (comparar-libertad))
      )

  )
)

(defrule libre
  (comparar-libertad)
  (respuesta-m21 ?l1)
  (respuesta-m22 ?l2)
  (Licencia (nombre ?l1) (libre ?f1))
  (Licencia (nombre ?l2) (libre ?f2))
  =>
  (if (= ?f2 0)
    then
      (printout t "La licencia " ?l2 " es privada no son compatibles" crlf)
    else
      (if (> ?f1 ?f2)
        then
          (printout t "Compatibles a nivel de libertad")
        else
          (printout t "No son compatibles a nivel de libertad" crlf)
      )
  )
)
