;______________________________________________________________________________;
;
;  personas.clp
;  Antonio Miguel Morillo Chica
;
;        Ejercicio para gestinar las relaciones presentes dentro de una
;   familia.
;
;        En mi implementación he intentado establecer unas pautas básicas.
;   Una tengo los nombres genero un hecho específico que provocará la
;   activación de la regla buscar relación que buscará la relación generada
;   previamente. Las primeras reglas que se disperan tras cargar los nombres
;   serían aquellas que no necesitan que exista una relación previa. Es decir,
;   a lo primero que aspiré es a resolver un pequeño nucleo familiar, padre,
;   madre, hijos. Se pueden, pues sacar una relación inicial, es-hijo y es
;   antecesor, tras esto deduzco quien es hermano, pareja..
;
;        Un hermano es el hijo de tu padres no siendo uno mismo. Un primo es
;   el hijo del hermano de tu padre, tu abuelo es el padre de tu padre, o,
;   visto de una forma descendente en el arbol genialogico, el nieto es el
;   hijo del hijo de alquien.
;______________________________________________________________________________;


; Template persona. Identifica a una persona cualquiera.
(deftemplate Persona
  (field nombre)      ; Al ser field obligo a que los
  (slot genero)       ; nombres no puedan tener  espacios :(
  (field conyuge)
  (field antecesor)
)

; Defino el formato que voy a generar de conocimiento, tipo guarda
; el nombre de la relacion, son-pareja, es-tio, es-primo.. y las
; personas aquellas que están implicadas.
(deftemplate Relacion
  (field Tipo)
  (field Persona1)
  (field Persona2)
  (field Persona3)	; Persona "intermediaria" de la relacion
)

; Familiares añadidos de una familia inventada.
(deffacts UnaFamilia
  (Persona (nombre Pedro_Castillo_Chica) (genero V) (antecesor Juan_Castillo_Perez) )
  (Persona (nombre Maria_Castillo_Chica) (genero M) (conyuge Roberto_Patera) (antecesor Juan_Castillo_Perez) )
  (Persona (nombre Roberto_Patera) (genero V) (conyuge Maria_Castillo_Chica))
  (Persona (nombre Maria_Castillo_Chica) (genero M) (antecesor Juan_Castillo_Perez) )
  (Persona (nombre Juan_Castillo_Perez) (genero V) (conyuge Maria_Chica_Pena) (antecesor Julio_Castillo_Cuevas) )
  (Persona (nombre Maria_Chica_Pena) (genero M) (conyuge Juan_Castillo_Perez))
  (Persona (nombre Julio_Castillo_Cuevas) (genero V) (conyuge Rocio_Perez_Morillo))
  (Persona (nombre Rocio_Perez_Morillo) (genero M) (conyuge Julio_Castillo_Cuevas))
  (Persona (nombre Pedro_Castillo_Perez) (genero V) (conyuge Juana_Romero_Gomez) (antecesor Julio_Castillo_Cuevas) )
  (Persona (nombre Juana_Romero_Gomez) (genero M) (conyuge Pedro_Castillo_Perez))
  (Persona (nombre Lucia_Castillo_Gomez) (genero M) (antecesor Pedro_Castillo_Perez) )
  (Persona (nombre Luana_Castillo_Gomez) (genero M) (antecesor Pedro_Castillo_Perez) )
  (Persona (nombre Maria_Castillo_Perez) (genero M) (antecesor Julio_Castillo_Cuevas) )
)

; Tambien funciona pero faltan personajes para que se
; establezcan relaciones mejores.
;
;(deffacts FamiliaFelix
;  (Persona (nombre Eddard_Stark)      (genero V) (conyuge Catelyn_Stark))
;  (Persona (nombre Catelyn_Stark)     (genero M) (conyuge Eddard_Stark))
;  (Persona (nombre Jeyne_Westerling)  (genero M) (conyuge Robb_Stark))
;  (Persona (nombre Benjen_Stark)      (genero V) (conyuge Lyanna_Stark))
;  (Persona (nombre Sansa_Stark)       (genero M)                            (antecesor Eddard_Stark))
;  (Persona (nombre Anya_Stark)        (genero M)                            (antecesor Eddard_Stark))
;  (Persona (nombre Brandom_Stark)     (genero V)                            (antecesor Eddard_Stark))
;  (Persona (nombre Rickon_Stark)      (genero V)                            (antecesor Eddard_Stark))
;  (Persona (nombre John_Snow)         (genero M)                            (antecesor Eddard_Stark))
;  (Persona (nombre Robb_Stark)        (genero V) (conyuge Jeyne_Westerling) (antecesor Eddard_Stark))
;  (Persona (nombre Lyanna_Stark)      (genero M) (conyuge Benjen_Stark)     (antecesor Richard_Stark))
;)

; Primera regla que pretendo que se active.
(defrule ObtenerNombres
  =>
  (printout t "Introduzca un nombre: ")
  (bind ?nombre1 (read))
  (printout t "Introduzca un nombre: ")
  (bind ?nombre2 (read))
  (assert (BuscarRelacion ?nombre1 ?nombre2))
)

; Regla que busca
(defrule BuscarRelacion
  (BuscarRelacion ?nombre1 ?nombre2)
  (Relacion (Tipo ?r) (Persona1 ?nombre1) (Persona2 ?nombre2) (Persona3 ?razon))
  (Persona (nombre ?nombre1) (genero ?G))
  =>
  ; Antecesor
  (if (eq ?r es-antecesor) then
    (if (eq ?G V) then
      (printout t ?nombre1 " es el padre de " ?nombre2 crlf)
    else
      (printout t ?nombre1 " es la madre de " ?nombre2 crlf)
    )
  )

  ; Hijos
  (if (eq ?r es-hijo) then
    (if (eq ?G V) then
      (printout t ?nombre1 " es el hijo de " ?nombre2 crlf)
    else
      (printout t ?nombre1 " es la hija de " ?nombre2 crlf)
    )
  )

  ; Pareja
  (if (eq ?r son-pareja) then
    (printout t ?nombre1 " y " ?nombre2 " son pareja " crlf)
  )

  ; Hermanos
  (if (eq ?r son-hermanos) then
    (printout t ?nombre1 " y " ?nombre2 " son hermanos " crlf)
    (printout t "Debido a que tienen los mismos antecesores" crlf)
  )

  ; Tios
  (if (eq ?r es-tio) then
    (if (eq ?G V) then
      (printout t ?nombre1 " es el tio de " ?nombre2 crlf)
      (printout t "Debido a que " ?nombre1 " es hermano de " ?razon crlf)
    else
      (printout t ?nombre1 " es la tia de " ?nombre2 crlf)
      (printout t "Debido a que " ?nombre1 " es hermana de " ?razon crlf)
    )
  )

  ; Sobrino
  (if (eq ?r es-sobrino) then
    (if (eq ?G V) then
      (printout t ?nombre1 " es el sobrino de " ?nombre2 crlf)
      (printout t "Debido a que " ?nombre1 " es hijo de " ?razon  ", " ?nombre2 " y " ?razon " son hermanaos " crlf)
      else
      (printout t ?nombre1 " es la sobrina de " ?nombre2 crlf)
      (printout t "Debido a que " ?nombre1 " es hija de " ?razon  ", " ?nombre2 " y " ?razon " son hermanaos " crlf)
    )
  )

  ; Primos
  (if (eq ?r son-primos) then
    (printout t ?nombre1 " y " ?nombre2 " son primos " crlf)
    (printout t "Debido a que " ?razon " es ti@ de " ?nombre1  " y " ?razon " es el antecesor de " ?nombre2 crlf)

  )

  ; Nietos
  (if (eq ?r es-nieto) then
    (if (eq ?G V) then
      (printout t ?nombre1 " es el nieto de " ?nombre2 crlf)
      else
      (printout t ?nombre1 " es la nieta de " ?nombre2 crlf)
    )
    (printout t "Debido a que " ?razon " es antecesor de " ?nombre1  " y " ?nombre2 " es antecesor de " ?razon crlf)
  )

  ; Abuelos
  (if (eq ?r es-abuelo) then
    (if (eq ?G V) then
      (printout t ?nombre1 " es el abuelo de " ?nombre2 crlf)
      else
      (printout t ?nombre1 " es la abuela de " ?nombre2 crlf)
    )
    (printout t "Debido a que " ?nombre1 " es antecesor de " ?razon " y " ?razon " es antecesor de " ?nombre2 crlf)
  )

  ; Cuñado
  (if (eq ?r son-cunados) then
    (printout t ?nombre1 " y " ?nombre2 " son cunados " crlf)
    (printout t "Debido a que " ?nombre1 " y " ?razon " son hermanos " crlf)
  )

  ; Suegro
  (if (eq ?r es-suegro) then
    (if (eq ?G V) then
      (printout t ?nombre1 " es el suegro de " ?nombre2 crlf)
      else
      (printout t ?nombre1 " es la suegra de " ?nombre2 crlf)
    )
  )

  ; Yerno
  (if (eq ?r es-yerno) then
    (if (eq ?G V) then
      (printout t ?nombre1 " es el yerno de " ?nombre2 crlf)
    else
      (printout t ?nombre1 " es la nuera de " ?nombre2 crlf)
    )
  )
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; P -> padre, M-> madre, h -> hijo
;; T -> tio,   S -> Suegro, A -> antecesor
;; AA -> "ante-antecesor", abuelo
;; C -> Cuñado
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Regla Padres o Famlia.
; Obtiene la relación padres, sea h un hijo y p  el padre, m la
; madre, h será hijo de p y m si exite un p con conyuge m y que además exista un
; h que tenga como antecesor a p o m.
; Se generaán 4 tipos de relaciones, es ancesor para cada padre y es hijo para
; cada padre.
(defrule Padres
  (Persona (nombre ?h) (antecesor ?P))
  (Persona (nombre ?P) (conyuge ?M))
  =>
  (assert (Relacion (Tipo es-antecesor)
                    (Persona1 ?P)
                    (Persona2 ?h))
  )
  (assert (Relacion (Tipo es-antecesor)
                    (Persona1 ?M)
                    (Persona2 ?h))
  )
  (assert (Relacion (Tipo es-hijo)
                    (Persona1 ?h)
                    (Persona2 ?P))
  )
  (assert (Relacion (Tipo es-hijo)
                    (Persona1 ?h)
                    (Persona2 ?M))
  )
)

; Regla Pareja.
; Obtiene la relacion sonpareja. Sea P y M dos personas distintas solo serán
; pareja si existe una persona que P que tenga conyuge M y que a la vez
; M sea una persona dentro del arbol genealgico.
;
(defrule Pareja
    (Persona (nombre ?P) (conyuge ?M))
    (Persona (nombre ?M) (conyuge ?P))
  =>
  (assert (Relacion (Tipo son-pareja)
                    (Persona1 ?P)
                    (Persona2 ?M))
  )
)

; Regla esHermano.
; Obtiene la relacion sonhermanos. Se H1 y H2 dos personas distintas y hermanas
; para que sean hermanos deben exisitr, obviamente, dos personas con el mismo
; padre y que la relacion SonFamilia esté establecida. Ha de haber pues, dos
; Relaciones SonFamilia cuya Persona1 y Persona2 son la misma pero Persona3, el
; hijo, han de ser distitnos.
;
(defrule Hermanos
  (Relacion (Tipo son-pareja)   (Persona1 ?P) (Persona2 ?M))
  (Relacion (Tipo es-antecesor) (Persona1 ?P) (Persona2 ?H1))
  (Relacion (Tipo es-antecesor) (Persona1 ?M) (Persona2 ?H2 & ~?H1)) ; Así no lo cojo dos veces.
  =>
  (assert (Relacion (Tipo son-hermanos)
                    (Persona1 ?H1)
                    (Persona2 ?H2))
  )
)

; Regla esTio.
; Obtiene la relación SonTipoSobrino. El tio es el hermano de uno de los padres.
; Buscamos pues una familia uniparental p, m y h, en donde p o m tiene un
; hermano, asumo que es imposible que p y m tengan un hermano común.
;
(defrule TioSobrino
  (Relacion (Tipo son-hermanos) (Persona1 ?A) (Persona2 ?T))
  (Relacion (Tipo es-antecesor) (Persona1 ?A) (Persona2 ?h))
  =>
  (assert (Relacion (Tipo es-tio)
                    (Persona1 ?T)
                    (Persona2 ?h)
                    (Persona3 ?A))
  )
  (assert (Relacion (Tipo es-sobrino)
                    (Persona1 ?h)
                    (Persona2 ?T)
                    (Persona3 ?A))
  )
)

; Regla esPrimo.
; Obtiene la relación SonPrimos.
; En esencia un primo sería, el hijo de tu tio por lo que tiene que haber
; un par de hijos que tengan

(defrule Primo
  (Relacion (Tipo es-tio)       (Persona1 ?T) (Persona2 ?S))
  (Relacion (Tipo es-antecesor) (Persona1 ?T) (Persona2 ?H))
  =>
  (assert (Relacion (Tipo son-primos)
                    (Persona1 ?S)
                    (Persona2 ?H)
                    (Persona3 ?T))
  )
)

; Regla Abuelo
; Obtiene dos relaciones es-abuelo y es-primo.
; El abuelo es el padre del padre de un hijo y el nieto es el hijo del hijo de
; cualquier persona.
;
(defrule Abuelo
  (Relacion (Tipo es-antecesor) (Persona1 ?AA) (Persona2 ?A))
  (Relacion (Tipo es-antecesor) (Persona1 ?A) (Persona2 ?h))
  =>
  (assert (Relacion (Tipo es-abuelo)
                    (Persona1 ?AA)
                    (Persona2 ?h)
                    (Persona3 ?A))
  )
  (assert (Relacion (Tipo es-nieto)
                    (Persona1 ?h)
                    (Persona2 ?AA)
                    (Persona3 ?A))
  )
)


; Regla Cunado
; Un cuñado es la pareja de tu hermano por lo que deberá exisir una Relacion
; ya entre hermanos en el que uno de ellos esté comprometido
;
(defrule Cunado
  (Relacion (Tipo son-hermanos) (Persona1 ?H1) (Persona2 ?H2))
  (Relacion (Tipo son-pareja)   (Persona1 ?H2) (Persona2 ?C))
  =>
  (assert (Relacion (Tipo es-cunado)
                    (Persona1 ?H1)
                    (Persona2 ?C)
                    (Persona3 ?H2))
  )
)

; Regla Suegro
; El suegro es la padre de tu pareja y el yerno es la pareja de un hijo
;
(defrule Suegro
  (Relacion (Tipo es-hijo)    (Persona1 ?h) (Persona2 ?P))
  (Relacion (Tipo son-pareja) (Persona1 ?h) (Persona2 ?pareja))
  =>
  (assert (Relacion (Tipo es-suegro)
                    (Persona1 ?P)
                    (Persona2 ?pareja)
                    (Persona3 ?h))
  )
  (assert (Relacion (Tipo es-yerno)
                    (Persona1 ?pareja)
                    (Persona2 ?P)
                    (Persona3 ?h))
  )
)












































;
