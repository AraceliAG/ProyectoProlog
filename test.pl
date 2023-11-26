
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


 resource(rPerro, image, image('Perro_resultado.jpg')).
 resource(rPinwino, image, image('Pinwino_resultado.jpg')).
 resource(rGato, image, image('Gato_resultado.jpg')).
 resource(rDelfin, image, image('Delfin_resultado.jpg')).
 resource(rVaca, image, image('Vaca_resultado.jpg')).


 resource(img_principal, image, image('menuu.jpg')). %FONDO DEL MENU 
 resource(portada, image, image('portada.jpg')). %FONDO DE INICIO 
 resource(adaptabilidad, image, image('Adaptabilidad.jpg')).
 resource(afectuosidad, image, image('Afectuosidad.jpg')).
 resource(alimentacion, image, image('Alimentacion.jpg')).
 resource(afecto, image, image('Afectuosidad.jpg')).
 resource(amigables, image, image('Amigables.jpg')).
 resource(cazador, image, image('Cazador.jpg')).
 resource(compromiso, image, image('Compromiso.jpg')).
 resource(comunicacion, image, image('Comunicacion.jpg')).
 resource(comunidad, image, image('Comunidad.jpg')).
 resource(crianza, image, image('Crianza.jpg')).
 resource(curiosidad, image, image('curiosidad.jpg')).
 resource(delfin, image, image('Delfin.jpg')).
 resource(delfin2, image, image('delfin2.jpg')).
 resource(empatia, image, image('Empatia.jpg')).
 resource(entrenables, image, image('Entrenables.jpg')).
 resource(gato, image, image('Gato.jpg')).
 resource(gato2, image, image('Gato2.jpg')).
 resource(independencia, image, image('Independencia.jpg')).
 resource(inteligencia, image, image('Inteligencia.jpg')).
 resource(jerarquia, image, image('Jerarquia.jpg')).
 resource(juego, image, image('Juego.jpg')).
 resource(juegos, image, image('Juegos.jpg')).
 resource(juguetones, image, image('Juguetones.jpg')).
 resource(lealtad, image, image('Lealtad.jpg')).
 resource(lenguaje, image, image('Lenguaje.jpg')).
 resource(perro, image, image('Perro.jpg')).
 resource(perro2, image, image('Perro2.jpg')).
 resource(perseveranciap, image, image('PersevaranciaP.jpg')).
 resource(pinwino, image, image('pinwino.jpg')).
 resource(pinwino2, image, image('Pinwino2.jpg')).
 resource(protectores, image, image('Protectores.jpg')).
 resource(resistencia, image, image('Resistencia.jpg')).
 resource(sociabilidad, image, image('Sociabilidad.jpg')).
 resource(tranquilidad, image, image('Tranquilidad.jpg')).
 resource(vaca, image, image('vaca.jpg')).
 resource(vaca2, image, image('vaca2.jpg')).

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
                                     send(Pantalla, display,Figura,point(0,0)).
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
                mostrar_resultado(Personaje),
                send(@texto, selection('Resultado Final:')),
                send(@main, display,@texto,point(20,10)),
                send(@resp1, selection(Personaje)),
                new(@boton, button('Nuevo test',
                message(@prolog, botones)
                )),

                new(@btntratamiento,button('Detalles',
                message(@prolog, mostrar_personaje,Personaje)
                )),
                send(@main, display,@boton,point(25,250)),
                send(@main, display,@btntratamiento,point(25,300)).   %BOTON DE DETALLES DEL PERSONAJE



  mostrar_personaje(X):-new(@tratam, dialog('')),
                          send(@tratam, append, label(nombre, '')),
                          send(@tratam, display,@lblExp1,point(0,0)),
                          send(@tratam, display,@lblExp2,point(0,0)),
                          tratamiento(X),
                          send(@tratam, transient_for, @main),
                          send(@tratam, open_centered).

/* AQUI SE MUESTRA EL PERSONAJE QUE ERES*/

tratamiento(X):- send(@lblExp1,selection('De acuerdo con el TEST eres:')),
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
  new(@texto, label(nombre,'ELIGE EL TEST QUE DESEAS REALIZAR',font('times','bold',20))), %ELIGE TEXTO
 
  %send(@texto, colour, white), %cambio de color de letra del label

  new(@resp1, label(nombre,'',font('times','bold',30))),  %MUESTRA EL MENSAJE DEL RESULTADO
  %send(@resp1, colour, white), %CAMBIO DE COLOR   %CAMBIO DE COLOR DEL RESULTADO
  new(@lblExp1, label(nombre,'',font('times','roman',14))),
  new(@lblExp2, label(nombre,'',font('times','roman',14))),
  new(@salir,button('SALIR',and(message(@main,destroy),message(@main,free)))),
  new(@boton, button('ANIMALES',message(@prolog, botones))), %Se crea boton ANIMALES


  

  new(@btntratamiento,button('Resultado')),

  nueva_imagen(@main, img_principal),
  send(@main, display,@boton,point(300,100)), %modificacion de coordenada de boton 
  send(@main, display,@texto,point(150,30)), %Posicion 
  send(@main, display,@salir,point(25,350)), 
  send(@main, display,@resp1,point(20,50)), %Posicion del resultado del personaje
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

conocimiento('rGato',
['Prefieres trabajar en proyectos de manera independiente', 'Te sientes atraido por explorar lugares o actividades nuevas',
'Disfrutas pasar tiempo con amigos y familiares cercanos','Te gusta participar en actividades ludicas','Tienes una mentalidad orientada a objetivos y te sientes motivado por lograr metas']).

conocimiento('rPerro',
['Consideras que eres una persona leal y confiable en tus relaciones personales y laborales', 'Te sientes comodo interactuando con nuevas personas y disfrutas de la compania de amigos y conocidos ',
'Tienes un espiritu jugueton y disfrutas de actividades recreativas y entretenimiento','Te sientes responsable por la seguridad y el bienestar de tus seres queridos','Eres receptivo a aprender nuevas habilidades y seguir instrucciones en situaciones personales o profesionales']).

conocimiento('rVaca',
['Sueles mantener la calma en situaciones de estres o prefieres ambientes pacificos y relajados',
'Tienes preferencias alimenticias especificas o eres adaptable a diferentes tipos de comidas y ambientes alimenticios', 'Eres habil en comunicar tus necesidades y emociones a traves del lenguaje corporal y la expresion verbal','Te consideras una persona resistente y capaz de afrontar desafios fisicos y climaticos con tenacidad','Te sientes comodo en situaciones con estructuras jerarquicas']).

conocimiento('rDelfin',
['Te consideras una persona que disfruta de desafios mentales y esta dispuesta a aprender y resolver problemas de manera efectiva', 'Te sientes atraido por la interaccion social y tiendes a formar relaciones cercanas con amigos y familiares',
 'Eres una persona que disfruta de la diversion y el juego en tu tiempo libre','Sientes que eres una persona empatica y capaz de comunicarte de manera efectiva con los demas','Tienes una mente curiosa y te sientes atraido por explorar cosas nuevas e inusuales en tu entorno']).

conocimiento('rPinwino',
['Te sientes mas comodo trabajando en equipo y disfrutas de la interaccion social o prefieres trabajar de manera independiente', 'Eres una persona que valora la monogamia y la fidelidad en las relaciones personales',
 'Te consideras una persona dedicada y dispuesta a asumir responsabilidades en el cuidado de tus seres queridos', 'Eres expresivo y comunicativo en tus relaciones con los demas o tiendes a ser mas reservado en tus expresiones','Eres capaz de adaptarte a situaciones desafiantes y mantener la resistencia en condiciones dificiles']).
%TEST_GATO
id_imagen_preg('Prefieres trabajar en proyectos de manera independiente','independencia'). %EN EL SEGUNDO PARAMETRO O SEA inndependiente SE LLAMA EL ID QUE SE DECLARA EN EL INICIO
id_imagen_preg('Te sientes atraido por explorar lugares o actividades nuevas','curiosidad').
id_imagen_preg('Disfrutas pasar tiempo con amigos y familiares cercanos','afectuosidad').
id_imagen_preg('Te gusta participar en actividades ludicas','juegos').
id_imagen_preg('Tienes una mentalidad orientada a objetivos y te sientes motivado por lograr metas','cazador').
%TEST_PERRO
id_imagen_preg('Consideras que eres una persona leal y confiable en tus relaciones personales y laborales','lealtad').
id_imagen_preg('Te sientes comodo interactuando con nuevas personas y disfrutas de la compania de amigos y conocidos','amigables').
id_imagen_preg('Tienes un espiritu jugueton y disfrutas de actividades recreativas y entretenimiento','juguetones').
id_imagen_preg('Te sientes responsable por la seguridad y el bienestar de tus seres queridos','protectores').
id_imagen_preg('Eres receptivo a aprender nuevas habilidades y seguir instrucciones en situaciones personales o profesionales','entrenables').
%TEST_VACA
id_imagen_preg('Sueles mantener la calma en situaciones de estres o prefieres ambientes pacificos y relajados','tranquilidad').
id_imagen_preg('Tienes preferencias alimenticias especificas o eres adaptable a diferentes tipos de comidas y ambientes alimenticios','alimentacion').
id_imagen_preg('Eres habil en comunicar tus necesidades y emociones a traves del lenguaje corporal y la expresion verbal','comunicacion').
id_imagen_preg('Te consideras una persona resistente y capaz de afrontar desafios fisicos y climaticos con tenacidad','resistencia').
id_imagen_preg('Te sientes comodo en situaciones con estructuras jerarquicas','jerarquia').
%TEST_DELFIN
id_imagen_preg('Te consideras una persona que disfruta de desafios mentales y esta dispuesta a aprender y resolver problemas de manera efectiva','inteligencia').
id_imagen_preg('Te sientes atraido por la interaccion social y tiendes a formar relaciones cercanas con amigos y familiares','sociabilidad').
id_imagen_preg('Eres una persona que disfruta de la diversion y el juego en tu tiempo libre','juego').
id_imagen_preg('Sientes que eres una persona empatica y capaz de comunicarte de manera efectiva con los demas','empatia').
id_imagen_preg('Tienes una mente curiosa y te sientes atraido por explorar cosas nuevas e inusuales en tu entorno','adaptabilidad').
%TEST_PINWINO
id_imagen_preg('Te sientes mas comodo trabajando en equipo y disfrutas de la interaccion social o prefieres trabajar de manera independiente','comunidad').
id_imagen_preg('Eres una persona que valora la monogamia y la fidelidad en las relaciones personales','compromiso').
id_imagen_preg('Te consideras una persona dedicada y dispuesta a asumir responsabilidades en el cuidado de tus seres queridos','crianza').
id_imagen_preg('Eres expresivo y comunicativo en tus relaciones con los demas o tiendes a ser mas reservado en tus expresiones','lenguaje').
id_imagen_preg('Eres capaz de adaptarte a situaciones desafiantes y mantener la resistencia en condiciones dificiles','perseveranciap').


 /* MOTOR DE INFERENCIA: Esta parte del sistema experto se encarga de
 inferir cual es el diagnostico a partir de las preguntas realizadas
 */
:- dynamic conocido/1.

  mostrar_resultado(X):-generar_personalidad(X),clean_scratchpad.
  mostrar_resultado(error):-clean_scratchpad .

  generar_personalidad(Personalidad):-
                            referente_respuestas(Personalidad, Lista),
                            prueba_presencia_de(Personalidad, Lista).


referente_respuestas(Personalidad, Lista):-
                            conocimiento(Personalidad, Lista).


prueba_presencia_de(Personalidad, []).
prueba_presencia_de(Personalidad, [Head | Tail]):- prueba_verdad_de(Personalidad, Head),
                                              prueba_presencia_de(Personalidad, Tail).


prueba_verdad_de(Personalidad, Res):- conocido(Res).
prueba_verdad_de(Personalidad, Res):- not(conocido(is_false(Res))),
pregunta_sobre(Personalidad, Res, Reply), Reply = 'si'.


pregunta_sobre(Personalidad, Res, Reply):- preguntar(Res,Respuesta),
                          process(Personalidad, Res, Respuesta, Reply).


process(Personalidad, Res, si, si):- asserta(conocido(Res)).
process(Personalidad, Res, no, no):- asserta(conocido(is_false(Res))).


clean_scratchpad:- retract(conocido(X)), fail.
clean_scratchpad.


conocido(_):- fail.

not(X):- X,!,fail.
not(_).
