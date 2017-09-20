;______________________________________________________________________________;
;
;  ejercicio8.clp | Heredar valores de slot
;  Antonio Miguel Morillo Chica
;
;  Crea un trozo de código en CLIPS (cuantas menos reglas se usen mejor) que
;  incluya el template.
;  Y que cuando un TipoAnimal t1 no tenga registrada la estructura, y exista
;  otro TipoAnimal t2 de forma que se encuentre en la memoria
;  (Es_un Nombre_de_t1 Nombre_de_t2), entonces copie el valor de Estructura
;  de t2 en t1. Por ejemplo, ante
;
;______________________________________________________________________________;


(deftemplate TipoAnimal
  (slot Nombre)
  (slot Estructura)
)
