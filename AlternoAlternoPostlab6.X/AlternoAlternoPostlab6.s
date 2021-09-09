 ; Archivo    :	  AlternoAlternoPostlab6.s  
 ; Dispositivo:	  PIC16F887
 ; Autor      :	  Alejandro Ramírez
 ; Compilador :	  MPLAB V5.4
 ; Programa   :	  Incrementar segundos con tmr1 y actualizar tasa de refresco de
		  ; ... displays con tmr0(2ms) y activar led con tmr2(500 ms)
 ; Hardware   :	  Led en el puerto e
 
 
 ; Última modificación: 31 agosto, 2021
 PROCESSOR 16F887
 #include <xc.inc>
 
 ;+------------------------------------------------+
 ;|              BITS DE CONFIGURACION             |
 ;+------------------------------------------------+  
 CONFIG FOSC  =   INTRC_NOCLKOUT	// Oscillador Interno sin salidas  
 CONFIG WDTE  =   OFF // WDT disabled 
 CONFIG PWRTE =   OFF // PWRT enabled 
 CONFIG MCLRE =   OFF // El pin de MCLR se utiliza como I/O
 CONFIG CP    =   OFF // Sin protección de código
 CONFIG CPD   =   OFF // Sin protección de datos

 CONFIG BOREN = OFF // Sin reinicio cuando el voltaje de alimentación baja de 4V
 CONFIG IESO  = OFF // Reinicio sin cambio de reloj de interno a externo
 CONFIG FCMEN = OFF // Cambio de reloj externo a interno en caso de fallo
 CONFIG LVP   = OFF // Programación en bajo voltaje permitida
 
 ;configuration word 2
 CONFIG WRT	= OFF// Protección de autoescritura por el programa desactivada
 CONFIG BOR4V	= BOR40V// Reinicio abajo de 4V, (BOR21V=2.1V)
 

 ;+------------------------------------------------+
 ;|                     MACROS                     |
 ;+------------------------------------------------+  
 restart_tmr0 macro ; Reseteo del tmr0
    banksel PORTA   ; Abrir banco PORTA
    movlw   6	    ; N = 61   -   t_deseado = (4*t_osc)(256-TMR0)(PRESCARLER)
    movwf   TMR0    ; Ciclo de 2ms
    bcf	    T0IF    ; Bandera de overflow de tmr0, del INTCON BIT 2
    endm
    
 restart_tmr1 macro ; Reseteo del tmr1
    movlw   0xC3    ; Precarga para el nibble significativo del tmr1
    movwf   TMR1H   
    movlw   0x40    ; Precarga para el nibble inferiror del tmr1
    movwf   TMR1L   ; Ciclo de 1s
    bcf	    TMR1IF  ; Bandera de overflow de tmr1
    endm
    
  restart_tmr2 macro; Reseteo del tmr1
    bcf TMR2IF	    ; Bandera de overflow de tmr2    
    endm
    
 wdivl	macro	divisor, cociente, residuo  ; Macro de divisor
    movwf   conteo	; El dividendo se encuentra en W, pasar w a conteo
    clrf    conteo+1	; Limpiar la variable que está sobre w

    incf    conteo+1	; Aumentar conteo + 1
    movlw   divisor	; Pasar la litera del divisor a w

    subwf   conteo, F	; restar de w conteo, y guardarlo en conteo 
    btfsc   STATUS,0	; Si carry 0, decrementar conteo+1
    goto    $-4

    decf    conteo+1, w ; Demcrementar conteo+1
    movwf   cociente	; Pasar el valor respectivo a cociente

    movlw   divisor	
    addwf   conteo, w
    movwf   residuo	; Pasar el valor respectivo a residuo
    endm
    
 ;+------------------------------------------------+
 ;|                  VARIABLES                     |
 ;+------------------------------------------------+  
 PSECT udata_bank0
    segundos:		DS 1
    mediosegundo:	DS 1
    conteo:		DS 3
    residuo1:		DS 1
    cociente1:		DS 1
    display_var:	DS 1
    display0:		DS 1
    display1:		DS 1
    
    
 PSECT udata_shr	; Memoria comun 
    W_TEMP:		DS  1	; 1 byte
    STATUS_TEMP:	DS  1	; 1 byte
 
 ;+------------------------------------------------+
 ;|                  VECTOR RESET                  |
 ;+------------------------------------------------+ 
 PSECT resVect, class=CODE, abs, delta=2
 ORG 00h	;posición 0000h para el reset
 resetVec:
     PAGESEL main
     goto main
     
 ;+------------------------------------------------+
 ;|              VECTOR INTERRUPCION               |
 ;+------------------------------------------------+
 PSECT resVect, class=CODE, abs, delta=2
 ORG 04h	; Posicion 0004h para las interrupciones
 
 ;+------------------------------------------------+
 ;|                 INTERRUPCIONES                 |
 ;+------------------------------------------------+
 push:
    movwf   W_TEMP	; Pasar el valor de W a la variable W_Temporal
    swapf   STATUS, W	; No se tocan las banderas, pero se hace un swap
    movwf   STATUS_TEMP ; Pasar el valor de status a status_temp
    
 isr:
    btfsc   T0IF	; Bandera de tmr0
    call    int_t0	; Interrupcion del tmr0
    
    btfsc   TMR1IF	; Bandera de tmr1
    call    int_t1	; Interrupcion del tmr1
    
    btfsc   TMR2IF	; Bandera de tmr2
    call    int_t2	; Interrupcion del tmr2
    
 pop:
    swapf   STATUS_TEMP	
    movwf   STATUS	
    swapf   W_TEMP, F	
    swapf   W_TEMP, W	
    retfie  ; Regreso de int

 ;+------------------------------------------------+
 ;|          SUBRUTINAS DE INTERRUPCIONES          |
 ;+------------------------------------------------+  
 int_t0: 
    restart_tmr0	    ; 2 ms   
    clrf    PORTD	    ; Limpiar PORTD
    btfsc   display_var,0   ; Variable aux de diplay, si no se va a diplay_0
    goto    display_1	    
    
 display_0:
    movf    display0, w	    ; Mover el valor de display0 a w
    movwf   PORTC	    ; Pasar de w a PORTC
    bsf	    PORTD, 0	    ; Configurar rd0, en 1
    goto    siguiente_display
    
 display_1:
    movf    display1, w	    ; Mover el valor de display1 a w
    movwf   PORTC	    ; Pasar de w a PORTC
    bsf	    PORTD, 1	    ; Configurar rd1, en 1
    goto    siguiente_display
    
 siguiente_display:	    ; Si display_var es 0, será 1 y al contario
    movlw   1
    xorwf   display_var, F
    return
       
 int_t1:
    restart_tmr1	; Limpiar tmr1
    incf    segundos	; Aumentar segundos
    return
    
  int_t2:
    restart_tmr2	; Limpiar tmr2
    incf    PORTE	; Incrementar porte
    return  
 
 ;+------------------------------------------------+
 ;|            POSICIÓN PARA EL CÓDIGO             |
 ;+------------------------------------------------+
 PSECT code, delta=2, abs
 ORG 100h	; posición para el código
 ;+------------------------------------------------+
 ;|                      TABLA                     | 
 ;+------------------------------------------------+  
 tabla_7seg:
    clrf    PCLATH
    bsf	    PCLATH, 0   ; 0100h
    andlw   0x0f
    addwf   PCL	; inst 103, PC 104 + 5
    retlw   00111111B       ;0
    retlw   00000110B	;1
    retlw   01011011B	;2
    retlw   01001111B	;3
    retlw   01100110B	;4
    retlw   01101101B	;5
    retlw   01111101B	;6
    retlw   00000111B	;7
    retlw   01111111B	;8
    retlw   01101111B	;9
    retlw   01110111B	; A
    retlw   01111100B	; B
    retlw   00111001B	; C
    retlw   01011110B	; D
    retlw   01111001B	; E
    retlw   01110001B	; F
 
 ;+------------------------------------------------+
 ;|                 CONFIGURACIONES                | 
 ;+------------------------------------------------+ 
 main:
    call    config_io   ; Portc y portd como salidas
    call    config_reloj; 500kHz
    call    config_tmr0	; Prescaler 1:8 ciclo de 1s
    call    config_tmr1	; Prescaler 1:2 ciclo de 2ms
    call    config_tmr2	; Pr 1:16, post 1:16 ciclo de 500ms
    call    config_int_enable	; Configuracion de timers
    banksel PORTA
    
 ;+------------------------------------------------+
 ;|                      LOOP                      | 
 ;+------------------------------------------------+  
 loop:
    call    display_configuracion ; Pasa los valores que cada display necesita 
    call    limite_display1	  ; Limitar display1
    goto    loop
    
 ;+------------------------------------------------+
 ;|                  SUB-RUTINAS                   | 
 ;+------------------------------------------------+ 
 config_io:
    banksel ANSEL	; Abrir los bancos de ANSEL y ANSELH   
    clrf    ANSEL	; P.Digitales
    clrf    ANSELH
    
    banksel TRISA	; Abrir el banco donde se encuentra TRISA
    ;clrf    TRISA	; port C - salida
    ;clrf    TRISB
    clrf    TRISC	; Port C - salida
    bcf	    TRISD,0	; Port RD0 - salida
    bcf	    TRISD,1	; Port RD1 - salida    
    bcf	    TRISE, 0	; Porte como salida
       
    banksel PORTA	; Abrir el banco de PORTA
    ;clrf    PORTA	; Limpiar el valor de PORTA
    ;clrf    PORTB
    clrf    PORTC	; Limpiar puerto c
    bcf	    PORTD,0	; Limpiar rd0  
    bcf	    PORTD,1	; Limpiar rd1
    bcf	    PORTE,0	; Limpiar re0
    
    return
 
config_reloj:		    ; 500 kHz
    banksel OSCCON
    bcf	    IRCF2	    ; OSCCON, 6
    bsf	    IRCF1	    ; OSCCON, 5
    bsf	    IRCF0	    ; OSCCON, 4
    bsf	    SCS		    
    return
    
config_tmr0:
    banksel TRISA   ; Abrir el OPTION_REG del banco donde se encuentra TRISA
    bcf	    T0CS    ; RELOJ INTERNO
    bcf	    PSA	    ; Prescaler
    bcf	    PS2
    bcf	    PS1
    bcf	    PS0	    ; PS = 000 = 1:2
    
    restart_tmr0
    return

config_tmr1:
    banksel PORTA
    bcf	    TMR1GE  ; siempre contando
    bsf	    T1CKPS1
    bsf	    T1CKPS0 ; Prescale2 de 1:8
    bcf	    T1OSCEN ; low power off
    bcf	    TMR1CS  ; Reloj externo apagado
    bsf	    TMR1ON  ; Activa TMR!
    ; Añadir los valore iniciales, con TMR1h y TMR1L
    restart_tmr1
    return

config_tmr2:
    banksel PORTA
    bsf	    TOUTPS3 ; 1
    bsf	    TOUTPS2 ; 1
    bsf	    TOUTPS1 ; 1
    bsf	    TOUTPS0 ; 1 Postscaler 1:16

    bsf	    TMR2ON  ; Timer 2 On

    bsf	    T2CKPS1 ;1
    bsf	    T2CKPS0 ;1 prescaler 1:16

    banksel TRISB   
    movlw   240	    ; Precarga para el PR2
    movwf   PR2	    ; Ciclo de 500 ms
    clrf    TMR2    
    clrf    TMR2IF  ; Limpiar bandera
    return
 
    
config_int_enable:  ; Configurar interrupción del tmr0
    banksel TRISA
    bsf	    TMR1IE  ; Interrupcion tmr1
    bsf	    TMR2IE  ; Interrupcion tmr2
    
    banksel PORTA
    bsf	    T0IE    ; Interrupcion tmr0
    bcf	    T0IF    ; Bandera tmro
    bcf	    TMR1IF  ; Bandera tmr1
    bcf	    TMR2IF  ; Bandera tmr2
    bsf	    PEIE    ; Interrupciones perifericas
    bsf	    GIE	    ; Interrupciones globales
    return 

display_configuracion: ; Pasa los valores que cada display necesita 
   movf    segundos, w		    ; Variable incrementada por tmr1
   wdivl   10, cociente1, residuo1  ; Divisor
    
   movf	    residuo1, w		    ; Pasar el residuo1 a w
   call	    tabla_7seg		    ; Pasar w a la tabla 
   movwf    display0		    ; Pasar w a display 0
   
   movf	    cociente1, w	    ; Pasar el cociente1 a w
   call	    tabla_7seg		    ; Pasar w a la tabla
   movwf    display1		    ; Pasar w a display 1
   return
   
limite_display1:	; Limitar display1 
    movf    segundos, w	; Pasar el valor de segundos a w
    sublw   100		; Restar 100 a W
    btfss   STATUS,2	; Si STATUS 2
    return		; Es 1, return
    clrf    segundos	; Es 0, limpiar segundos
    return	

END