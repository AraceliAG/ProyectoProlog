
/*
INTERFAZ GRAFICA: Esta parte del sistema experto es la que se encarga de
interactuar con la persona comun, mostrar imagenes, botones, textos, etc.

INICIAR SISTEMA EXPERTO:
PARA CORRER EL PROGRAMA, ES NESESARIO CARGAR LAS 3 PARTES AL SWI PROLOG
Y LUEGO SOLO CONSULTAR TODO, AUTOMATICAMENTE SE ABRIRA LA VENTANA DEL PROGRAMA
*/
 :- use_module(library(pce)).
 :- pce_image_directory('./imagenes').
 :- use_module(library(pce_style_item)).
 :- dynamic color/2.

 resource(img_principal, image, image('xd.jpg')).
 resource(portada, image, image('xd.jpg')).
 resource(hidropesia, image, image('delfin2.jpg')).
 resource(vejiga_natatoria, image, image('pinwino.jpg')).
 resource(punto_blanco_ich, image, image('pinwino2.jpg')).
 resource(estres, image, image('delfin2.jpg')).
 resource(parasito_hexamita, image, image('delfin2.jpg')).
 resource(lo_siento_diagnostico_desconocido, image, image('delfin2.jpg')).
 resource(agresivo, image, image('delfin2.jpg')).
 resource(aletargamiento, image, image('delfin2.jpg')).
 resource(aletas_retraidas, image, image('delfin2.jpg')).
 resource(equilibrio, image, image('delfin2.jpg')).
 resource(escamas_levantadas, image, image('delfin2.jpg')).
 resource(falta_apetito, image, image('delfin2.jpg')).
 resource(hexamita, image, image('delfin2.jpg')).
 resource(hexamita2, image, image('delfin2.jpg')).
 resource(ich, image, image('delfin2.jpg')).
 resource(ojos_sobresalidos, image, image('delfin2.jpg')).
 resource(venas_rojas, image, image('delfin2.jpg')).
 resource(vientre_hinchado, image, image('delfin2.jpg')).

 mostrar_imagen(Pantalla, Imagen) :- new(Figura, figure),
                                     new(Bitmap, bitmap(resource(Imagen),@on)),
                                     send(Bitmap, name, 1),
                                     send(Figura, display, Bitmap),
                                     send(Figura, status, 1),
                                     send(Pantalla, display,Figura,point(100,80)).
  mostrar_imagen_tratamiento(Pantalla, Imagen) :-new(Figura, figure),
                                     new(Bitmap, bitmap(resource(Imagen),@on)),
                                     send(Bitmap, name, 1),
                                     send(Figura, display, Bitmap),
                                     send(Figura, status, 1),
                                     send(Pantalla, display,Figura,point(20,100)).
 nueva_imagen(Ventana, Imagen) :-new(Figura, figure),
                                new(Bitmap, bitmap(resource(Imagen),@on)),
                                send(Bitmap, name, 1),
                                send(Figura, display, Bitmap),
                                send(Figura, status, 1),
                                send(Ventana, display,Figura,point(0,0)).
  imagen_pregunta(Ventana, Imagen) :-new(Figura, figure),
                                new(Bitmap, bitmap(resource(Imagen),@on)),
                                send(Bitmap, name, 1),
                                send(Figura, display, Bitmap),
                                send(Figura, status, 1),
                                send(Ventana, display,Figura,point(500,60)).
 %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%5
  botones:-borrado,
                send(@boton, free),
                send(@btntratamiento,free),
                mostrar_diagnostico(Enfermedad),
                send(@texto, selection('El Diagnostico a partir de los datos es:')),
                send(@resp1, selection(Enfermedad)),
                new(@boton, button('Nuevo test',
                message(@prolog, botones)
                )),

                new(@btntratamiento,button('Detalles',
                message(@prolog, mostrar_tratamiento,Enfermedad)
                )),
                send(@main, display,@boton,point(20,450)),
                send(@main, display,@btntratamiento,point(138,450)).



  mostrar_tratamiento(X):-new(@tratam, dialog('Características')),
                          send(@tratam, append, label(nombre, 'Explicacion: ')),
                          send(@tratam, display,@lblExp1,point(70,51)),
                          send(@tratam, display,@lblExp2,point(50,80)),
                          tratamiento(X),
                          send(@tratam, transient_for, @main),
                          send(@tratam, open_centered).

tratamiento(X):- send(@lblExp1,selection('De Acuerdo Al Diagnostico El Tratamiento Es:')),
                 mostrar_imagen_tratamiento(@tratam,X).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


   preguntar(Preg,Resp):-new(Di,dialog('Colsultar Datos:')),
                        new(L2,label(texto,'Responde las siguientes preguntas')),
                        id_imagen_preg(Preg,Imagen),
                        imagen_pregunta(Di,Imagen),
                        new(La,label(prob,Preg)),
                        new(B1,button(si,and(message(Di,return,si)))),
                        new(B2,button(no,and(message(Di,return,no)))),
                        send(Di, gap, size(25,25)),
                        send(Di,append(L2)),
                        send(Di,append(La)),
                        send(Di,append(B1)),
                        send(Di,append(B2)),
                        send(Di,default_button,'si'),
                        send(Di,open_centered),get(Di,confirm,Answer),
                        free(Di),
                        Resp=Answer.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

interfaz_principal:-new(@main,dialog('TEST',
  size(1000,1000))),
  new(@texto, label(nombre,'ELIGE EL TEST QUE DESEAS REALIZAR',font('times','bold',20))),
 
  send(@texto, colour, white), %cambio de color de letra del label

  new(@resp1, label(nombre,'',font('times','roman',22))),
  new(@lblExp1, label(nombre,'',font('times','roman',14))),
  new(@lblExp2, label(nombre,'',font('times','roman',14))),
  new(@salir,button('SALIR',and(message(@main,destroy),message(@main,free)))),
  new(@boton, button('ANIMALES',message(@prolog, botones))), %Se crea boton ANIMALES
  new(@boton2, button('FNAF',message(@prolog, botones))), %Se crea boton FNAF
  new(@boton3, button('TIPO DE ESTUDIANTES',message(@prolog, botones))), %Se crea boton tipo de estudiante
  

  new(@btntratamiento,button('¿Tratamiento?')),

  nueva_imagen(@main, img_principal),
  send(@main, display,@boton,point(350,200)), %modificacion de coordenada de boton 
  send(@main, display,@boton2,point(350,250)), %modificacion de coordenada de boton2
  send(@main, display,@boton3,point(350,300)), %modificacion de boton 3
  send(@main, display,@texto,point(150,130)),
  send(@main, display,@salir,point(320,350)),
  send(@main, display,@resp1,point(20,180)),
  send(@main,open_centered).

 borrado:- send(@resp1, selection('')).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

  crea_interfaz_inicio:- new(@interfaz,dialog('Bienvenido al Test',
  size(1000,1000))),

  mostrar_imagen(@interfaz, portada),

  new(BotonComenzar,button('COMENZAR',and(message(@prolog,interfaz_principal) ,
  and(message(@interfaz,destroy),message(@interfaz,free)) ))),
  new(BotonSalir,button('SALIDA',and(message(@interfaz,destroy),message(@interfaz,free)))),
  send(@interfaz,append(BotonComenzar)),
  send(@interfaz,append(BotonSalir)),
  send(@interfaz,open_centered).

  :-crea_interfaz_inicio.

/* BASE DE CONOCIMIENTOS: caracteristicas de personalidad, contiente ademas
el identificador de imagenes de acuerdo a las repuestas
*/

conocimiento('ERES UN GATO',
['Prefieres trabajar en proyectos de manera independiente o en equipo', 'Te sientes atraido por explorar lugares o actividades nuevas',
'Disfrutas pasar tiempo con amigos y familiares cercanos','Te gusta participar en actividades ludicas o juegos en tu tiempo libre']).

conocimiento('ERES UN PERRO',
['Te gusta participar en actividades ludicas', 'Tienes una mentalidad orientada a objetivos y te sientes motivado por lograr metas ',
'Disfrutas pasar tiempo con amigos y familiares cercanos','Consideras que eres una persona leal y confiable en tus relaciones personales y laborales']).

conocimiento('punto_blanco_ich',['Te sientes responsable por la seguridad y el bienestar de tus seres queridos y estas dispuesto a protegerlos',
'Consideras que eres una persona leal y confiable en tus relaciones personales y laborales', 'Tienes un espiritu jugueton y disfrutas de actividades recreativas y entretenimiento']).

conocimiento('estres',
['Te sientes responsable por la seguridad y el bienestar de tus seres queridos y estas dispuesto a protegerlos', 'Disfrutas pasar tiempo con amigos y familiares cercanos',
 'Consideras que eres una persona leal y confiable en tus relaciones personales y laborales','Eres receptivo a aprender nuevas habilidades y seguir instrucciones en situaciones personales o profesionales']).

conocimiento('parasito_hexamita',
['el pez tiene un hoyo en la cabeza', 'Disfrutas pasar tiempo con amigos y familiares cercanos',
 'Consideras que eres una persona leal y confiable en tus relaciones personales y laborales', 'el pez tiene la cabeza con sangre y tejido muerto']).
%TEST_GATO
id_imagen_preg('Prefieres trabajar en proyectos de manera independiente o en equipo','escamas_levantadas').
id_imagen_preg('Te sientes atraido por explorar lugares o actividades nuevas','ojos_sobresalidos').
id_imagen_preg('Disfrutas pasar tiempo con amigos y familiares cercanos','falta_apetito').
id_imagen_preg('Te gusta participar en actividades ludicas o juegos en tu tiempo libre','vientre_hinchado').
id_imagen_preg('Tienes una mentalidad orientada a objetivos y te sientes motivado por lograr metas','equilibrio').
%TEST_PERRO
id_imagen_preg('Consideras que eres una persona leal y confiable en tus relaciones personales y laborales','aletargamiento').
id_imagen_preg('Te sientes comodo interactuando con nuevas personas y disfrutas de la compania de amigos y conocidos','ich').
id_imagen_preg('Tienes un espiritu jugueton y disfrutas de actividades recreativas y entretenimiento','aletas_retraidas').
id_imagen_preg('Te sientes responsable por la seguridad y el bienestar de tus seres queridos y estas dispuesto a protegerlos','agresivo').
id_imagen_preg('Eres receptivo a aprender nuevas habilidades y seguir instrucciones en situaciones personales o profesionales','venas_rojas').
%TEST_VACA
id_imagen_preg('Sueles mantener la calma en situaciones de estrés o prefieres ambientes pacificos y relajados','hexamita').
id_imagen_preg('Tienes preferencias alimenticias especificas o eres adaptable a diferentes tipos de comidas y ambientes alimenticios','hexamita2').
id_imagen_preg('Eres habil en comunicar tus necesidades y emociones a traves del lenguaje corporal y la expresión verbal','hexamita').
id_imagen_preg('Te consideras una persona resistente y capaz de afrontar desafios fisicos y climaticos con tenacidad','hexamita').
id_imagen_preg('Te sientes comodo en situaciones con estructuras jerarquicas o prefieres un entorno mas igualitario','hexamita').
%TEST_DELFIN
id_imagen_preg('Te consideras una persona que disfruta de desafios mentales y esta dispuesta a aprender y resolver problemas de manera efectiva','hexamita').
id_imagen_preg('Te sientes atraido por la interaccion social y tiendes a formar relaciones cercanas con amigos y familiares','hexamita').
id_imagen_preg('Eres una persona que disfruta de la diversion y el juego en tu tiempo libre','hexamita').
id_imagen_preg('Sientes que eres una persona empatica y capaz de comunicarte de manera efectiva con los demas','hexamita').
id_imagen_preg('Tienes una mente curiosa y te sientes atraido por explorar cosas nuevas e inusuales en tu entorno','hexamita').
%TEST_PINWINO
id_imagen_preg('Te sientes mas comodo trabajando en equipo y disfrutas de la interacción social o prefieres trabajar de manera independiente','hexamita').
id_imagen_preg('Eres una persona que valora la monogamia y la fidelidad en las relaciones personales','hexamita').
id_imagen_preg('Te consideras una persona dedicada y dispuesta a asumir responsabilidades en la crianza de tus hijos o cuidado de tus seres queridos','hexamita').
id_imagen_preg('Eres expresivo y comunicativo en tus relaciones con los demas o tiendes a ser mas reservado en tus expresiones','hexamita').
id_imagen_preg('Eres capaz de adaptarte a situaciones desafiantes y mantener la resistencia en condiciones dificiles','hexamita').
 /* MOTOR DE INFERENCIA: Esta parte del sistema experto se encarga de
 inferir cual es el diagnostico a partir de las preguntas realizadas
 */
:- dynamic conocido/1.

  mostrar_diagnostico(X):-haz_diagnostico(X),clean_scratchpad.
  mostrar_diagnostico(lo_siento_diagnostico_desconocido):-clean_scratchpad .

  haz_diagnostico(Diagnosis):-
                            obten_hipotesis_y_sintomas(Diagnosis, ListaDeSintomas),
                            prueba_presencia_de(Diagnosis, ListaDeSintomas).


obten_hipotesis_y_sintomas(Diagnosis, ListaDeSintomas):-
                            conocimiento(Diagnosis, ListaDeSintomas).


prueba_presencia_de(Diagnosis, []).
prueba_presencia_de(Diagnosis, [Head | Tail]):- prueba_verdad_de(Diagnosis, Head),
                                              prueba_presencia_de(Diagnosis, Tail).


prueba_verdad_de(Diagnosis, Sintoma):- conocido(Sintoma).
prueba_verdad_de(Diagnosis, Sintoma):- not(conocido(is_false(Sintoma))),
pregunta_sobre(Diagnosis, Sintoma, Reply), Reply = 'si'.


pregunta_sobre(Diagnosis, Sintoma, Reply):- preguntar(Sintoma,Respuesta),
                          process(Diagnosis, Sintoma, Respuesta, Reply).


process(Diagnosis, Sintoma, si, si):- asserta(conocido(Sintoma)).
process(Diagnosis, Sintoma, no, no):- asserta(conocido(is_false(Sintoma))).


clean_scratchpad:- retract(conocido(X)), fail.
clean_scratchpad.


conocido(_):- fail.

not(X):- X,!,fail.
not(_).
