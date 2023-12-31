
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

 resource(img_principal, image, image('xd2.jpg')). %FONDO DEL MENU 
 resource(portada, image, image('xd.jpg')). %FONDO DE INICIO 
 resource(comunicacion, image, image('Comunicacion.jpg')).
 resource(jueg, image, image('Juego.jpg')).
 resource(lenguaje, image, image('Lenguaje.jpg')).
 resource(jerarquia, image, image('Jerarquia.jpg')).
 resource(empatia, image, image('Empatia.jpg')).
 resource(persevera, image, image('PersevaranciaP.jpg')).
 resource(crianza, image, image('Crianza.jpg')).
 resource(comunidad, image, image('Comunidad.jpg')).
 resource(compromiso, image, image('Compromiso.jpg')).
 resource(social, image, image('Sociabilidad.jpg')).
 resource(parasito_hexamita, image, image('delfin2.jpg')).
 resource(lo_siento_diagnostico_desconocido, image, image('delfin2.jpg')).
 resource(protector, image, image('Protectores.jpg')).
 resource(lealtad, image, image('Lealtad.jpg')).
 resource(jugueton, image, image('Juguetones.jpg')).
 resource(cazador, image, image('Cazador.jpg')).
 resource(independiente, image, image('Independencia.jpg')). %SE AGREGA EL NOMBRE DEL ID EN ESTE CASO ES independiente Y DENTRO DEL PARENTESIS EL NOMBRE DE NUETRO ARCHIVO
 resource(afecto, image, image('Afectuosidad.jpg')).
 resource(calma, image, image('Tranquilidad.jpg')).
  resource(resistencia, image, image('Resistencia.jpg')).
 resource(alimentacion, image, image('Alimentacion.jpg')).
 resource(amistoso, image, image('Perro.jpg')).
 resource(curiosidad, image, image('curiosidad.jpg')).
 resource(habilidad, image, image('Perro2.jpg')).
 resource(juegos, image, image('Juegos.jpg')).

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
                mostrar_resultado(Personaje),
                send(@texto, selection('Resultado Final:')),
                send(@resp1, selection(Personaje)),
                new(@boton, button('Nuevo test',
                message(@prolog, botones)
                )),

                new(@btntratamiento,button('Detalles',
                message(@prolog, mostrar_personaje,Personaje)
                )),
                send(@main, display,@boton,point(25,350)),
                send(@main, display,@btntratamiento,point(25,300)).   %BOTON DE DETALLES DEL PERSONAJE



  mostrar_personaje(X):-new(@tratam, dialog('Características')),
                          send(@tratam, append, label(nombre, 'Explicacion: ')),
                          send(@tratam, display,@lblExp1,point(70,51)),
                          send(@tratam, display,@lblExp2,point(50,80)),
                          tratamiento(X),
                          send(@tratam, transient_for, @main),
                          send(@tratam, open_centered).

/* AQUI SE MUESTRA EL PERSONAJE QUE ERES*/

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
  new(@texto, label(nombre,'ELIGE EL TEST QUE DESEAS REALIZAR',font('times','bold',20))), %ELIGE TEXTO
 
  send(@texto, colour, white), %cambio de color de letra del label

  new(@resp1, label(nombre,'',font('times','bold',30))),  %MUESTRA EL MENSAJE DEL RESULTADO
  send(@resp1, colour, white), %CAMBIO DE COLOR 
  new(@lblExp1, label(nombre,'',font('times','roman',14))),
  new(@lblExp2, label(nombre,'',font('times','roman',14))),
  new(@salir,button('SALIR',and(message(@main,destroy),message(@main,free)))),
  new(@boton, button('ANIMALES',message(@prolog, botones))), %Se crea boton ANIMALES
  new(@boton2, button('FNAF',message(@prolog, botones))), %Se crea boton FNAF

  

  new(@btntratamiento,button('Resultado')),

  nueva_imagen(@main, img_principal),
  send(@main, display,@boton,point(350,200)), %modificacion de coordenada de boton 
  send(@main, display,@boton2,point(350,250)), %modificacion de coordenada de boton2
  send(@main, display,@texto,point(150,130)), %Posicion del resultado del personaje
  send(@main, display,@salir,point(350,350)), 
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

conocimiento('ERES UN GATO',
['Prefieres trabajar en proyectos de manera independiente', 'Te sientes atraido por explorar lugares o actividades nuevas',
'Disfrutas pasar tiempo con amigos y familiares cercanos','Te gusta participar en actividades ludicas','Tienes una mentalidad orientada a objetivos y te sientes motivado por lograr metas']).

conocimiento('ERES UN PERRO',
['Consideras que eres una persona leal y confiable en tus relaciones personales y laborales', 'Te sientes comodo interactuando con nuevas personas y disfrutas de la compania de amigos y conocidos ',
'Tienes un espiritu jugueton y disfrutas de actividades recreativas y entretenimiento','Te sientes responsable por la seguridad y el bienestar de tus seres queridos','Eres receptivo a aprender nuevas habilidades y seguir instrucciones en situaciones personales o profesionales']).

conocimiento('ERES UNA VACA',
['Sueles mantener la calma en situaciones de estrés o prefieres ambientes pacificos y relajados',
'Tienes preferencias alimenticias especificas o eres adaptable a diferentes tipos de comidas y ambientes alimenticios', 'Eres habil en comunicar tus necesidades y emociones a traves del lenguaje corporal y la expresión verbal','Te consideras una persona resistente y capaz de afrontar desafios fisicos y climaticos con tenacidad','Te sientes comodo en situaciones con estructuras jerarquicas']).

conocimiento('ERES UN DELFIN',
['Te consideras una persona que disfruta de desafios mentales y esta dispuesta a aprender y resolver problemas de manera efectiva', 'Te sientes atraido por la interaccion social y tiendes a formar relaciones cercanas con amigos y familiares',
 'Eres una persona que disfruta de la diversion y el juego en tu tiempo libre','Sientes que eres una persona empatica y capaz de comunicarte de manera efectiva con los demas','Tienes una mente curiosa y te sientes atraido por explorar cosas nuevas e inusuales en tu entorno']).

conocimiento('ERES UN PINWINO',
['Te sientes mas comodo trabajando en equipo y disfrutas de la interacción social o prefieres trabajar de manera independiente', 'Eres una persona que valora la monogamia y la fidelidad en las relaciones personales',
 'Te consideras una persona dedicada y dispuesta a asumir responsabilidades en la crianza de tus hijos o cuidado de tus seres queridos', 'Eres expresivo y comunicativo en tus relaciones con los demas o tiendes a ser mas reservado en tus expresiones','Eres capaz de adaptarte a situaciones desafiantes y mantener la resistencia en condiciones dificiles']).

%TEST_FNAF
conocimiento('ERES FOXY',
['Prefieres abordar los desafios de frente y actuar de manera decidida incluso cuando la situación es intensa', 'Te consideras alguien que a pesar de los contratiempos puede recuperarse y seguir adelante con resiliencia y determinacion',
'Disfrutas de la interaccion social directa siendo claro y directo en tus comunicaciones incluso en situaciones intensas','Eres bueno para adaptarte a circunstancias cambiantes y para mantenerte fuerte y en funcionamiento a pesar de desafíos constantes','Eres mas competitivo y auto-suficiente o prefieres colaborar y trabajar en conjunto']).
conocimiento('ERES FREDDYFAZBEAR',
['Te sientes comodo tomando la iniciativa y liderando equipos en situaciones desafiantes o complejas', 'Disfrutas de estar en el centro de la atencion o prefieres mantener un perfil mas discreto en grupos sociales',
'Eres bueno manejando situaciones impredecibles o cambiantes manteniendo la calma y tomando decisiones rapidas','Eres colaborativo o puedes tener rivalidades competitivas','Te mantienes firme o prefieres evitar conflictos']).
conocimiento('ERES CHICA POLLITA SEXY',
['Te identificas como alguien con una actitud energica y positiva que tiende a contagiar alegria en su entorno', 'Disfrutas participando activamente en grupos sociales siendo dinamico/a y animado/a en tu interaccion con otras personas',
'Consideras importante proyectar una imagen amigable y colorida en tu forma de vestir o en tu presentacion personal','Tienes rasgos o habitos que te hacen destacar ya sea por tu comportamiento predecible o por algo unico que te distingue','Eres alguien que tiende a tener relaciones especificas con ciertos individuos o que interactua de manera equitativa con todos']).
conocimiento('ERES BONNIE',
['Te sientes atraido por actividades creativas como la musica el arte o cualquier expresion artistica', 'Te identificas mas como alguien que aunque puede ser timido en ciertas situaciones muestra una personalidad mas activa o expresiva en contextos especificos',
'Tienes rasgos o habilidades que te hacen destacar ya sea en tu apariencia o en alguna actividad que te apasione','Te sientes mas comodo/a interactuando con un grupo selecto de personas o te sientes a gusto socializando con una amplia variedad de individuos','Eres mas cercano/a a ciertas personas o mantienes relaciones equitativas con todos']).
conocimiento('ERES SPRINGTRAP',
['Eres capaz de mantener la calma y tomar decisiones racionales', 'Tienes habilidades para lidiar con situaciones inesperadas o sorpresivas manteniendo la compostura y tomando medidas rapidas y eficaces',
'Te sientes atraido por el misterio y lo oscuro en historias películas o juegos o prefieres evitarlos por completo','Eres una persona que tiende a planificar estrategicamente en situaciones dificiles o inquietantes o prefieres actuar espontaneamente','Eres capaz de mantener la calma y tomar decisiones efectivas o te sientes abrumado/a']).




%TEST_GATO
id_imagen_preg('Prefieres trabajar en proyectos de manera independiente','independiente'). %EN EL SEGUNDO PARAMETRO O SEA inndependiente SE LLAMA EL ID QUE SE DECLARA EN EL INICIO
id_imagen_preg('Te sientes atraido por explorar lugares o actividades nuevas','curiosidad').
id_imagen_preg('Disfrutas pasar tiempo con amigos y familiares cercanos','afecto').
id_imagen_preg('Te gusta participar en actividades ludicas','juegos').
id_imagen_preg('Tienes una mentalidad orientada a objetivos y te sientes motivado por lograr metas','cazador').
%TEST_PERRO
id_imagen_preg('Consideras que eres una persona leal y confiable en tus relaciones personales y laborales','lealtad').
id_imagen_preg('Te sientes comodo interactuando con nuevas personas y disfrutas de la compania de amigos y conocidos','amistoso').
id_imagen_preg('Tienes un espiritu jugueton y disfrutas de actividades recreativas y entretenimiento','jugueton').
id_imagen_preg('Te sientes responsable por la seguridad y el bienestar de tus seres queridos','protector').
id_imagen_preg('Eres receptivo a aprender nuevas habilidades y seguir instrucciones en situaciones personales o profesionales','habilidad').
%TEST_VACA
id_imagen_preg('Sueles mantener la calma en situaciones de estrés o prefieres ambientes pacificos y relajados','calma').
id_imagen_preg('Tienes preferencias alimenticias especificas o eres adaptable a diferentes tipos de comidas y ambientes alimenticios','alimentacion').
id_imagen_preg('Eres habil en comunicar tus necesidades y emociones a traves del lenguaje corporal y la expresión verbal','comunicacion').
id_imagen_preg('Te consideras una persona resistente y capaz de afrontar desafios fisicos y climaticos con tenacidad','resistencia').
id_imagen_preg('Te sientes comodo en situaciones con estructuras jerarquicas','jerarquia').
%TEST_DELFIN
id_imagen_preg('Te consideras una persona que disfruta de desafios mentales y esta dispuesta a aprender y resolver problemas de manera efectiva','calma').
id_imagen_preg('Te sientes atraido por la interaccion social y tiendes a formar relaciones cercanas con amigos y familiares','social').
id_imagen_preg('Eres una persona que disfruta de la diversion y el juego en tu tiempo libre','jueg').
id_imagen_preg('Sientes que eres una persona empatica y capaz de comunicarte de manera efectiva con los demas','social').
id_imagen_preg('Tienes una mente curiosa y te sientes atraido por explorar cosas nuevas e inusuales en tu entorno','curiosidad').
%TEST_PINWINO
id_imagen_preg('Te sientes mas comodo trabajando en equipo y disfrutas de la interacción social o prefieres trabajar de manera independiente','comunidad').
id_imagen_preg('Eres una persona que valora la monogamia y la fidelidad en las relaciones personales','compromiso').
id_imagen_preg('Te consideras una persona dedicada y dispuesta a asumir responsabilidades en la crianza de tus hijos o cuidado de tus seres queridos','crianza').
id_imagen_preg('Eres expresivo y comunicativo en tus relaciones con los demas o tiendes a ser mas reservado en tus expresiones','lenguaje').
id_imagen_preg('Eres capaz de adaptarte a situaciones desafiantes y mantener la resistencia en condiciones dificiles','persevera').


%TEST_FOXY
id_imagen_preg('Prefieres abordar los desafios de frente y actuar de manera decidida incluso cuando la situación es intensa','escamas_levantadas').
id_imagen_preg('Te consideras alguien que a pesar de los contratiempos puede recuperarse y seguir adelante con resiliencia y determinacion','calma').
id_imagen_preg('Disfrutas de la interaccion social directa siendo claro y directo en tus comunicaciones incluso en situaciones intensas','calma').
id_imagen_preg('Eres bueno para adaptarte a circunstancias cambiantes y para mantenerte fuerte y en funcionamiento a pesar de desafíos constantes','calma').
id_imagen_preg('Eres mas competitivo y auto-suficiente o prefieres colaborar y trabajar en conjunto','calma').
%TEST_FREDDYFAZBEAR
id_imagen_preg('Te sientes comodo tomando la iniciativa y liderando equipos en situaciones desafiantes o complejas','calma').
id_imagen_preg('Disfrutas de estar en el centro de la atencion o prefieres mantener un perfil mas discreto en grupos sociales','calma').
id_imagen_preg('Eres bueno manejando situaciones impredecibles o cambiantes manteniendo la calma y tomando decisiones rapidas','calma').
id_imagen_preg('Eres colaborativo o puedes tener rivalidades competitivas','calma').
id_imagen_preg('Te mantienes firme o prefieres evitar conflictos','calma').
%TEST_CHICA
id_imagen_preg('Te identificas como alguien con una actitud energica y positiva que tiende a contagiar alegria en su entorno','calma').
id_imagen_preg('Disfrutas participando activamente en grupos sociales siendo dinamico/a y animado/a en tu interaccion con otras personas','calma').
id_imagen_preg('Consideras importante proyectar una imagen amigable y colorida en tu forma de vestir o en tu presentacion personal','calma').
id_imagen_preg('Tienes rasgos o habitos que te hacen destacar ya sea por tu comportamiento predecible o por algo unico que te distingue','calma').
id_imagen_preg('Eres alguien que tiende a tener relaciones especificas con ciertos individuos o que interactua de manera equitativa con todos','calma').
%TEST_BONNIE
id_imagen_preg('Te sientes atraido por actividades creativas como la musica el arte o cualquier expresion artistica','calma').
id_imagen_preg('Te identificas mas como alguien que aunque puede ser timido en ciertas situaciones muestra una personalidad mas activa o expresiva en contextos especificos','calma').
id_imagen_preg('Tienes rasgos o habilidades que te hacen destacar ya sea en tu apariencia o en alguna actividad que te apasione','calma').
id_imagen_preg('Te sientes mas comodo/a interactuando con un grupo selecto de personas o te sientes a gusto socializando con una amplia variedad de individuos','calma').
id_imagen_preg('Eres mas cercano/a a ciertas personas o mantienes relaciones equitativas con todos','calma').
%TEST_SPRINGTRAP
id_imagen_preg('Eres capaz de mantener la calma y tomar decisiones racionales','hexamita').
id_imagen_preg('Tienes habilidades para lidiar con situaciones inesperadas o sorpresivas manteniendo la compostura y tomando medidas rapidas y eficaces','calma').
id_imagen_preg('Te sientes atraido por el misterio y lo oscuro en historias películas o juegos o prefieres evitarlos por completo','calma').
id_imagen_preg('Eres una persona que tiende a planificar estrategicamente en situaciones dificiles o inquietantes o prefieres actuar espontaneamente','calma').
id_imagen_preg('Eres capaz de mantener la calma y tomar decisiones efectivas o te sientes abrumado/a','calma').



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
