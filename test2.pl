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
%ANIMALES
 resource(img_principal, image, image('xd2.jpg')). %FONDO DEL MENU 
 resource(portada, image, image('xd.jpg')). %FONDO DE INICIO 
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
%FNAF
 /resource(agresividad, image, image('AgresividadFoxy.jpg')).
 resource(aspaterrador, image, image('AspAterrador.jpg')).
 resource(aspdistintivo, image, image('AspDistintivo.jpg')).
 resource(colaboracion, image, image('Colaboracion.jpg')).
 resource(comppeligroso, image, image('CompPeligroso.jpg')).
 resource(dinamicas, image, image('Dinamicas.jpg')).
 resource(energia, image, image('Energia.jpg')).
 resource(escenarios, image, image('Escenarios.jpg')).
 resource(impredecible, image, image('Impredecible.jpg')).
 resource(interacion, image, image('Interacion.jpg')).
 resource(intimidacion, image, image('Intimidacion.jpg')).
 resource(intmalevola, image, image('IntMalevola.jpg')).
 resource(liderazgo, image, image('Liderazgo.jpg')).
 resource(mantenimiento, image, image('Mantenimiento.jpg')).
 resource(movdistintos, image, image('MovDistintos.jpg')).
 resource(musica, image, image('Musica.jpg')).
 resource(obsesion, image, image('Obsesion.jpg')).
 resource(obstaculos, image, image('Obstaculos.jpg')).
 resource(presentacion, image, image('Presentacion.jpg')).
 resource(sigilo, image, image('sigilo.jpg')).
 resource(penudo, image, image('Timido.jpg'))./

 
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
  imagen_pregunta1(Ventana, Imagen) :-new(Figura, figure),
                                new(Bitmap, bitmap(resource(Imagen),@on)),
                                send(Bitmap, name, 1),
                                send(Figura, display, Bitmap),
                                send(Figura, status, 1),
                                send(Ventana, display,Figura,point(500,60)).
 %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  botonesA:-borradoA,
                send(@boton1, free),
                send(@btntratamiento,free),
                mostrar_resultadoA(PersonajeA),
                send(@texto, selection('Resultado Final:')),
                send(@main, display,@texto,point(20,10)),
                send(@resp1, selection(PersonajeA)),
                new(@boton1, button('Nuevo test',
                message(@prolog, botonesA)
                )),

                new(@btntratamiento,button('Detalles',
                message(@prolog, mostrar_personajeA,PersonajeA)
                )),
                send(@main, display,@boton1,point(25,250)),
                send(@main, display,@btntratamiento,point(25,300)).   %BOTON DE DETALLES DEL PERSONAJE
 %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
 /botones:-borrado,
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
                send(@main, display,@btntratamiento,point(25,300)).   %BOTON DE DETALLES DEL PERSONAJE/
  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  mostrar_personajeA(X):-new(@tratam, dialog('Características')),
                          send(@tratam, append, label(nombre, 'Explicacion: ')),
                          send(@tratam, display,@lblExp1,point(70,51)),
                          send(@tratam, display,@lblExp2,point(50,80)),
                          tratamientoA(X),
                          send(@tratam, transient_for, @main),
                          send(@tratam, open_centered).

/* AQUI SE MUESTRA EL PERSONAJE QUE ERES*/

tratamientoA(X):- send(@lblExp1,selection('De Acuerdo Al Diagnostico El Tratamiento Es:')),
                 mostrar_imagen_tratamiento(@tratam,X).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
/mostrar_personaje(X):-new(@tratam, dialog('Características')),
                          send(@tratam, append, label(nombre, 'Explicacion: ')),
                          send(@tratam, display,@lblExp1,point(70,51)),
                          send(@tratam, display,@lblExp2,point(50,80)),
                          tratamiento(X),
                          send(@tratam, transient_for, @main),
                          send(@tratam, open_centered).

/* AQUI SE MUESTRA EL PERSONAJE QUE ERES*/

/tratamiento(X):- send(@lblExp1,selection('De Acuerdo Al Diagnostico El Tratamiento Es:')),
                 mostrar_imagen_tratamiento(@tratam,X)./
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

   preguntar1(PregA,RespA):-new(Di,dialog('Colsultar Datos:')),
                        new(L2,label(texto,'Responde las siguientes preguntas')),
                        id_imagen_pregA(PregA,Imagen),
                        imagen_pregunta1(Di,Imagen),
                        new(La,label(prob,PregA)),
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
                        RespA=Answer.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

      /preguntar(Preg,Resp):-new(Di,dialog('Colsultar Datos:')),
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
                        Resp=Answer./
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

interfaz_principal:-new(@main,dialog('TEST',
  size(1000,1000))),
  new(@texto, label(nombre,'ELIGE EL TEST QUE DESEAS REALIZAR',font('times','bold',20))), %ELIGE TEXTO
 
  send(@texto, colour, white), %cambio de color de letra del label

  new(@resp1, label(nombre,'',font('times','bold',30))),  %MUESTRA EL MENSAJE DEL RESULTADO
  send(@resp1, colour, white), %CAMBIO DE COLOR   %CAMBIO DE COLOR DEL RESULTADO
  new(@lblExp1, label(nombre,'',font('times','roman',14))),
  new(@lblExp2, label(nombre,'',font('times','roman',14))),
  new(@salir,button('SALIR',and(message(@main,destroy),message(@main,free)))),
  new(@boton1, button('ANIMALES',message(@prolog, botonesA))), %Se crea boton ANIMALES
  /new(@boton, button('FNAF',message(@prolog, botones))), %Se crea boton FNAF/
  new(@btntratamiento,button('Resultado')),

  nueva_imagen(@main, img_principal),
  send(@main, display,@boton1,point(300,100)), %modificacion de coordenada de boton 
  /send(@main, display,@boton,point(300,100)), %modificacion de coordenada de boton/
  send(@main, display,@texto,point(150,30)), %Posicion 
  send(@main, display,@salir,point(25,350)), 
  send(@main, display,@resp1,point(20,50)), %Posicion del resultado del personaje
  send(@main,open_centered).

 borradoA:- send(@resp1, selection('')).
 /borrado:- send(@resp1, selection(''))./
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
conocimientoA('ERES UN GATO',
['Prefieres trabajar en proyectos de manera independiente', 'Te sientes atraido por explorar lugares o actividades nuevas',
'Disfrutas pasar tiempo con amigos y familiares cercanos','Te gusta participar en actividades ludicas','Tienes una mentalidad orientada a objetivos y te sientes motivado por lograr metas']).

conocimientoA('ERES UN PERRO',
['Consideras que eres una persona leal y confiable en tus relaciones personales y laborales', 'Te sientes comodo interactuando con nuevas personas y disfrutas de la compania de amigos y conocidos ',
'Tienes un espiritu jugueton y disfrutas de actividades recreativas y entretenimiento','Te sientes responsable por la seguridad y el bienestar de tus seres queridos','Eres receptivo a aprender nuevas habilidades y seguir instrucciones en situaciones personales o profesionales']).

conocimientoA('ERES UNA VACA',
['Sueles mantener la calma en situaciones de estres o prefieres ambientes pacificos y relajados',
'Tienes preferencias alimenticias especificas o eres adaptable a diferentes tipos de comidas y ambientes alimenticios', 'Eres habil en comunicar tus necesidades y emociones a traves del lenguaje corporal y la expresion verbal','Te consideras una persona resistente y capaz de afrontar desafios fisicos y climaticos con tenacidad','Te sientes comodo en situaciones con estructuras jerarquicas']).

conocimientoA('ERES UN DELFIN',
['Te consideras una persona que disfruta de desafios mentales y esta dispuesta a aprender y resolver problemas de manera efectiva', 'Te sientes atraido por la interaccion social y tiendes a formar relaciones cercanas con amigos y familiares',
 'Eres una persona que disfruta de la diversion y el juego en tu tiempo libre','Sientes que eres una persona empatica y capaz de comunicarte de manera efectiva con los demas','Tienes una mente curiosa y te sientes atraido por explorar cosas nuevas e inusuales en tu entorno']).

conocimientoA('ERES UN PINWINO',
['Te sientes mas comodo trabajando en equipo y disfrutas de la interaccion social o prefieres trabajar de manera independiente', 'Eres una persona que valora la monogamia y la fidelidad en las relaciones personales',
 'Te consideras una persona dedicada y dispuesta a asumir responsabilidades en el cuidado de tus seres queridos', 'Eres expresivo y comunicativo en tus relaciones con los demas o tiendes a ser mas reservado en tus expresiones','Eres capaz de adaptarte a situaciones desafiantes y mantener la resistencia en condiciones dificiles']).

%TEST_GATO
id_imagen_pregA('Prefieres trabajar en proyectos de manera independiente','independencia'). %EN EL SEGUNDO PARAMETRO O SEA inndependiente SE LLAMA EL ID QUE SE DECLARA EN EL INICIO
id_imagen_pregA('Te sientes atraido por explorar lugares o actividades nuevas','curiosidad').
id_imagen_pregA('Disfrutas pasar tiempo con amigos y familiares cercanos','afectuosidad').
id_imagen_pregA('Te gusta participar en actividades ludicas','juegos').
id_imagen_pregA('Tienes una mentalidad orientada a objetivos y te sientes motivado por lograr metas','cazador').
%TEST_PERRO
id_imagen_pregA('Consideras que eres una persona leal y confiable en tus relaciones personales y laborales','lealtad').
id_imagen_pregA('Te sientes comodo interactuando con nuevas personas y disfrutas de la compania de amigos y conocidos','amigables').
id_imagen_pregA('Tienes un espiritu jugueton y disfrutas de actividades recreativas y entretenimiento','juguetones').
id_imagen_pregA('Te sientes responsable por la seguridad y el bienestar de tus seres queridos','protectores').
id_imagen_pregA('Eres receptivo a aprender nuevas habilidades y seguir instrucciones en situaciones personales o profesionales','entrenables').
%TEST_VACA
id_imagen_pregA('Sueles mantener la calma en situaciones de estres o prefieres ambientes pacificos y relajados','tranquilidad').
id_imagen_pregA('Tienes preferencias alimenticias especificas o eres adaptable a diferentes tipos de comidas y ambientes alimenticios','alimentacion').
id_imagen_pregA('Eres habil en comunicar tus necesidades y emociones a traves del lenguaje corporal y la expresion verbal','comunicacion').
id_imagen_pregA('Te consideras una persona resistente y capaz de afrontar desafios fisicos y climaticos con tenacidad','resistencia').
id_imagen_pregA('Te sientes comodo en situaciones con estructuras jerarquicas','jerarquia').
%TEST_DELFIN
id_imagen_pregA('Te consideras una persona que disfruta de desafios mentales y esta dispuesta a aprender y resolver problemas de manera efectiva','inteligencia').
id_imagen_pregA('Te sientes atraido por la interaccion social y tiendes a formar relaciones cercanas con amigos y familiares','sociabilidad').
id_imagen_pregA('Eres una persona que disfruta de la diversion y el juego en tu tiempo libre','juego').
id_imagen_pregA('Sientes que eres una persona empatica y capaz de comunicarte de manera efectiva con los demas','empatia').
id_imagen_pregA('Tienes una mente curiosa y te sientes atraido por explorar cosas nuevas e inusuales en tu entorno','adaptabilidad').
%TEST_PINWINO
id_imagen_pregA('Te sientes mas comodo trabajando en equipo y disfrutas de la interaccion social o prefieres trabajar de manera independiente','comunidad').
id_imagen_pregA('Eres una persona que valora la monogamia y la fidelidad en las relaciones personales','compromiso').
id_imagen_pregA('Te consideras una persona dedicada y dispuesta a asumir responsabilidades en el cuidado de tus seres queridos','crianza').
id_imagen_pregA('Eres expresivo y comunicativo en tus relaciones con los demas o tiendes a ser mas reservado en tus expresiones','lenguaje').
id_imagen_pregA('Eres capaz de adaptarte a situaciones desafiantes y mantener la resistencia en condiciones dificiles','perseveranciap').


%TEST_FNAF
/conocimiento('ERES FOXY',
['Prefieres abordar los desafios de frente y actuar de manera decidida incluso cuando la situación es intensa', 'Te consideras alguien que a pesar de los contratiempos puede recuperarse y seguir adelante con resiliencia y determinacion',
'Disfrutas de la interaccion social directa siendo claro y directo en tus comunicaciones incluso en situaciones intensas','Eres bueno para adaptarte a circunstancias cambiantes y para mantenerte fuerte y en funcionamiento a pesar de desafíos constantes','Eres mas competitivo y auto-suficiente o prefieres colaborar y trabajar en conjunto']).
conocimiento('ERES FREDDYFAZBEAR',
['Te sientes comodo tomando la iniciativa y liderando equipos en situaciones desafiantes o complejas', 'Disfrutas de estar en el centro de la atencion o prefieres mantener un perfil mas discreto en grupos sociales',
'Eres bueno manejando situaciones impredecibles o cambiantes manteniendo la calma y tomando decisiones rapidas','Eres colaborativo o puedes tener rivalidades competitivas','Te mantienes firme o prefieres evitar conflictos']).
conocimiento('ERES CHICA POLLITA SEXY',
['Te identificas como alguien con una actitud energica y positiva que tiende a contagiar alegria en su entorno', 'Disfrutas participando activamente en grupos sociales siendo dinamico y animado en tu interaccion con otras personas',
'Consideras importante proyectar una imagen amigable y colorida en tu forma de vestir o en tu presentacion personal','Tienes rasgos o habitos que te hacen destacar ya sea por tu comportamiento predecible o por algo unico que te distingue','Eres alguien que tiende a tener relaciones especificas con ciertos individuos o que interactua de manera equitativa con todos']).
conocimiento('ERES BONNIE',
['Te sientes atraido por actividades creativas como la musica el arte o cualquier expresion artistica', 'Te identificas mas como alguien que aunque puede ser timido en ciertas situaciones muestra una personalidad mas activa o expresiva en contextos especificos',
'Tienes rasgos o habilidades que te hacen destacar ya sea en tu apariencia o en alguna actividad que te apasione','Te sientes mas comodo interactuando con un grupo selecto de personas o te sientes a gusto socializando con una amplia variedad de individuos','Eres mas cercano a ciertas personas o mantienes relaciones equitativas con todos']).
conocimiento('ERES SPRINGTRAP',
['Eres capaz de mantener la calma y tomar decisiones racionales', 'Tienes habilidades para lidiar con situaciones inesperadas o sorpresivas manteniendo la compostura y tomando medidas rapidas y eficaces',
'Te sientes atraido por el misterio y lo oscuro en historias películas o juegos o prefieres evitarlos por completo','Eres una persona que tiende a planificar estrategicamente en situaciones dificiles o inquietantes o prefieres actuar espontaneamente','Eres capaz de mantener la calma y tomar decisiones efectivas o te sientes abrumado/a'])./

%TEST_FOXY
/id_imagen_preg('Prefieres abordar los desafios de frente y actuar de manera decidida incluso cuando la situación es intensa','agresividad').
id_imagen_preg('Te consideras alguien que a pesar de los contratiempos puede recuperarse y seguir adelante con resiliencia y determinacion','obstaculos').
id_imagen_preg('Disfrutas de la interaccion social directa siendo claro y directo en tus comunicaciones incluso en situaciones intensas','interacion').
id_imagen_preg('Eres bueno para adaptarte a circunstancias cambiantes y para mantenerte fuerte y en funcionamiento a pesar de desafíos constantes','mantenimiento').
id_imagen_preg('Eres mas competitivo y auto-suficiente o prefieres colaborar y trabajar en conjunto','colaboracion').
%TEST_FREDDYFAZBEAR
id_imagen_preg('Te sientes comodo tomando la iniciativa y liderando equipos en situaciones desafiantes o complejas','liderazgo').
id_imagen_preg('Disfrutas de estar en el centro de la atencion o prefieres mantener un perfil mas discreto en grupos sociales','escenarios').
id_imagen_preg('Eres bueno manejando situaciones impredecibles o cambiantes manteniendo la calma y tomando decisiones rapidas','impredecible').
id_imagen_preg('Eres colaborativo o puedes tener rivalidades competitivas','dinamicas').
id_imagen_preg('Te mantienes firme o prefieres evitar conflictos','intimidacion').
%TEST_CHICA
id_imagen_preg('Te identificas como alguien con una actitud energica y positiva que tiende a contagiar alegria en su entorno','energia').
id_imagen_preg('Disfrutas participando activamente en grupos sociales siendo dinamico y animado en tu interaccion con otras personas','escenarios').
id_imagen_preg('Consideras importante proyectar una imagen amigable y colorida en tu forma de vestir o en tu presentacion personal','presentacion').
id_imagen_preg('Tienes rasgos o habitos que te hacen destacar ya sea por tu comportamiento predecible o por algo unico que te distingue','impredecible').
id_imagen_preg('Eres alguien que tiende a tener relaciones especificas con ciertos individuos o que interactua de manera equitativa con todos','dinamicas').
%TEST_BONNIE
id_imagen_preg('Te sientes atraido por actividades creativas como la musica el arte o cualquier expresion artistica','musica').
id_imagen_preg('Te identificas mas como alguien que aunque puede ser timido en ciertas situaciones muestra una personalidad mas activa o expresiva en contextos especificos','movdistintos').
id_imagen_preg('Tienes rasgos o habilidades que te hacen destacar ya sea en tu apariencia o en alguna actividad que te apasione','penudo').
id_imagen_preg('Te sientes mas comodo interactuando con un grupo selecto de personas o te sientes a gusto socializando con una amplia variedad de individuos','aspdistintivo').
id_imagen_preg('Eres mas cercano a ciertas personas o mantienes relaciones equitativas con todos','escenarios').
%TEST_SPRINGTRAP
id_imagen_preg('Eres capaz de mantener la calma y tomar decisiones racionales','aspaterrador').
id_imagen_preg('Tienes habilidades para lidiar con situaciones inesperadas o sorpresivas manteniendo la compostura y tomando medidas rapidas y eficaces','comppeligroso').
id_imagen_preg('Te sientes atraido por el misterio y lo oscuro en historias películas o juegos o prefieres evitarlos por completo','obsesion').
id_imagen_preg('Eres una persona que tiende a planificar estrategicamente en situaciones dificiles o inquietantes o prefieres actuar espontaneamente','sigilo').
id_imagen_preg('Eres capaz de mantener la calma y tomar decisiones efectivas o te sientes abrumado/a','intmalevola')./

/* MOTOR DE INFERENCIA: Esta parte del sistema experto se encarga de
 inferir cual es el diagnostico a partir de las preguntas realizadas
 */

:- dynamic conocidoA/1.

  mostrar_resultadoA(X):-generar_personalidadA(X),clean_scratchpad.
  mostrar_resultadoA(error):-clean_scratchpad .

  generar_personalidadA(PersonalidadA):-
                            referente_respuestasA(PersonalidadA, Lista),
                            prueba_presencia_deA(PersonalidadA, Lista).


referente_respuestasA(PersonalidadA, Lista):-
                            conocimientoA(PersonalidadA, Lista).


prueba_presencia_deA(PersonalidadA, []).
prueba_presencia_deA(PersonalidadA, [Head | Tail]):- prueba_verdad_deA(PersonalidadA, Head),
                                              prueba_presencia_deA(PersonalidadA, Tail).


prueba_verdad_deA(PersonalidadA, Res):- conocidoA(Res).
prueba_verdad_deA(PersonalidadA, Res):- not(conocidoA(is_false(Res))),
pregunta_sobreA(PersonalidadA, Res, Reply), Reply = 'si'.


pregunta_sobreA(PersonalidadA, Res, Reply):- preguntar1(Res,RespuestaA),
                          process(PersonalidadA, Res, RespuestaA, Reply).


process(PersonalidadA, Res, si, si):- asserta(conocidoA(Res)).
process(PersonalidadA, Res, no, no):- asserta(conocidoA(is_false(Res))).


clean_scratchpad:- retract(conocidoA(X)), fail.
clean_scratchpad.


conocidoA(_):- fail.

not(X):- X,!,fail.
not(_).

/:- dynamic conocido/1.

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
not(_)./
