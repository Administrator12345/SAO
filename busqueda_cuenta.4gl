schema "dsipe"
GLOBALS "sao_variables_globales.4gl"

FUNCTION consulta_nominapp()

DEFINE lsqlCuentaSir STRING 
DEFINE ldateFecFinCal DATE 
DEFINE ldateFecIniCal DATE 

OPEN WINDOW w_06  WITH FORM "Fecha_cuenta" 
      INPUT BY NAME ldateFecFinCal WITHOUT DEFAULTS     

    BEFORE INPUT 
    LET ldateFecFinCal ="" 
      DISPLAY BY NAME ldateFecFinCal 

    AFTER INPUT 
       IF NOT INT_FLAG THEN 
            LET lsqlCuentaSir="select fecha_ini_semana  FROM c_calendar_sir where fecha_fin_semana=?"    
            PREPARE prelsqlCuentaSir FROM lsqlCuentaSir
            EXECUTE prelsqlCuentaSir USING ldateFecFinCal INTO ldateFecIniCal
            DISPLAY "ldateFecIniCal",ldateFecIniCal
            IF (ldateFecIniCal IS NULL OR ldateFecIniCal <='31/12/1899' ) THEN 
                CALL FGL_WINMESSAGE("Generación de Nómina","Esa fecha no existe en el calendario de Pagos","INFORMATION") 
                EXIT INPUT
            ELSE 
                CALL datos_nominapp(ldateFecFinCal,ldateFecIniCal)
            END IF 
       END IF 
    END INPUT 

    CLOSE WINDOW w_06 
END FUNCTION 

FUNCTION datos_nominapp(ldateFecFinCal,ldateFecIniCal)
   DEFINE list11 RECORD
        id_cuenta_sir       INTEGER,
        spe_id           	INTEGER,
        num_tarjeta       	VARCHAR(20),
        clabe            	VARCHAR(18),
        desc_deleg         	VARCHAR(100),
        num_pension      	INTEGER,
        num_issste          INTEGER,
        neto_pagar   	DECIMAL(12,2),
        fecha_proceso    	DATE,
        reg_enviado      	INTEGER,
        --fecha_enviado    	DATE,
        --reg_transferido  	INTEGER,
        --fecha_transferido	DATE,
        --reg_cancelado    	INTEGER,
        --fecha_cancelado  	DATE,
        usuario          	CHAR(8),
        seleccion      	    INTEGER
        --fecha_aud        	DATE,
        --hora_aud         	CHAR(8),
        --componente_cve   	CHAR(8),
        --ip_maquina       	CHAR(15)
END RECORD

    DEFINE list1 DYNAMIC ARRAY OF RECORD
        id_cuenta_sir       INTEGER,
        spe_id           	INTEGER,
        num_tarjeta       	VARCHAR(20),
        clabe            	VARCHAR(18),
        desc_deleg         	VARCHAR(100),
        num_pension      	INTEGER,
        num_issste          INTEGER,
        neto_pagar   	DECIMAL(12,2),
        fecha_proceso    	DATE,
        reg_enviado      	INTEGER,
        --fecha_enviado    	DATE,
        --reg_transferido  	INTEGER,
        --fecha_transferido	DATE,
        --reg_cancelado    	INTEGER,
        --fecha_cancelado  	DATE,
        usuario          	CHAR(8),
        seleccion      	    INTEGER
        --fecha_aud        	DATE,
        --hora_aud         	CHAR(8),
        --componente_cve   	CHAR(8),
        --ip_maquina       	CHAR(15)
END RECORD
    DEFINE 
        id_cuenta_sir1       INTEGER,
        spe_id1           	INTEGER,
        num_tarjeta1       	VARCHAR(20),
        clabe1            	VARCHAR(18),
        desc_deleg         	VARCHAR(100),
        num_pension      	INTEGER,
        num_issste          INTEGER,
        neto_pagar1  	DECIMAL(12,2),
        fecha_proceso    	DATE,
        reg_enviado1      	INTEGER,
        fecha_enviado1    	DATE,
        reg_transferido1  	INTEGER,
        fecha_transferido1	DATE,
        reg_cancelado1    	INTEGER,
        fecha_cancelado1  	DATE,
        usuario          	CHAR(8),
        fecha_aud1        	DATE,
        hora_aud1         	CHAR(8),
        componente_cve1   	CHAR(8),
        ip_maquina1       	CHAR(15),
        seleccion,li_suma      	    INTEGER


   DEFINE query,ls_where 	STRING
   DEFINE rowCurrent INTEGER
   DEFINE contUsuario INTEGER
   DEFINE tmpUsuario  VARCHAR(8)
   DEFINE i ,iterador          INTEGER
   DEFINE checbox,lsi_while,lsm_guar,lsm_actual SMALLINT 
   DEFINE windowactiva ui.window
   DEFINE formaactiva ui.form
   
   DEFINE usuari VARCHAR(30)
   DEFINE cve_deleg SMALLINT 
   DEFINE cve_perfil VARCHAR(10)
   DEFINE ldateFecFinCal DATE 
   DEFINE ldateFecIniCal DATE
    
    LET gusuari = ARG_VAL(5)
    LET gcve_deleg = ARG_VAL(2)
   
   DISPLAY "El usuario", gusuari
   DISPLAY "la DELEGACION ES :",gcve_deleg

    
   
   SET ISOLATION TO DIRTY READ

    LET query= "select usuario,cve_deleg,cve_perfil from operadores where cve_operador = ",gcve_deleg
    DISPLAY query
    PREPARE pre_count3 FROM query
    EXECUTE pre_count3 INTO usuari,cve_deleg,cve_perfil
    DISPLAY usuari," -- ",cve_deleg," -- ",cve_perfil
    
   DISPLAY "Consulta de Dispersion de Cheques"
   DISPLAY "Prueba4"
   
   OPEN WINDOW frmConsultaCuenta  WITH FORM "consulta_disper_cheque"

        LET li_suma=0
            LET checbox = TRUE 
            IF cve_deleg = 40 THEN 
                DISPLAY "delegacion 40"
			LET query="SELECT cs.id_cuenta_sir,
                                                cs.spe_id,
                                                cs.num_tarjeta,
                                                cs.clabe, 
                                                cd.desc_deleg,
                                                cs.num_pension,
                                                cs.num_issste,
                                                cs.neto_pagar,
                                                cs.fecha_proceso,
                                                cs.reg_enviado,
                                                cs.usuario
                                         FROM cuenta_sir cs, c_deleg cd
                                         WHERE fecha_enviado is null
                                         AND cs.dis_cve = cd.cve_deleg 
                                         AND fecha_proceso BETWEEN to_date('", ldateFecIniCal, "','%d/%m/%Y') 
                                         AND to_date('",ldateFecFinCal,"','%d/%m/%Y')"
                ELSE
        LET query = "SELECT cs.id_cuenta_sir,
                                cs.spe_id,
                                cs.num_tarjeta,
                                cs.clabe,
                                cd.desc_deleg,
                                cs.num_pension,
                                cs.num_issste,
                                cs.neto_pagar,
                                cs.fecha_proceso,
                                cs.reg_enviado,
                                cs.usuario
                         FROM cuenta_sir cs, c_deleg cd
                         WHERE fecha_enviado is null
                         AND cs.dis_cve = cd.cve_deleg
                         AND cs.dis_cve = ", cve_deleg, " 
                         AND fecha_proceso BETWEEN to_date('", ldateFecIniCal, "','%d/%m/%Y') 
                         AND to_date('",ldateFecFinCal,"','%d/%m/%Y')"
                END IF
                              DISPLAY "query: " , query
                              PREPARE prepareQuery FROM query
                              DECLARE cursorCuenta CURSOR FOR  prepareQuery
                              DISPLAY "abriendo cursor"
                              OPEN cursorCuenta
                              DISPLAY "limpiando array"
                              CALL list1.clear()
                               
                              FOREACH cursorCuenta INTO list11.*
                                      CALL list1.appendElement()
                                      LET list1[list1.getLength()].* = list11.*
                                      DISPLAY list11.*
                              END FOREACH

        
   
	LET windowactiva = ui.window.getcurrent()
	LET formaactiva = windowactiva.getform()
    --INPUT BY NAME desc_deleg,fecha_proceso, num_issste, usuario, num_pension
    INPUT BY NAME desc_deleg, fecha_proceso, num_issste, usuario,num_pension ATTRIBUTES (UNBUFFERED,CANCEL=FALSE)
   {}
                    BEFORE INPUT 
                        LET query=""
                        --CALL list1.clear()

                    AFTER INPUT
                        IF Valida_Integer(num_issste) = FALSE THEN
                            CALL mensaje("El número ISSSTE debe contener dígitos entre 0 y 9")
                            NEXT FIELD num_issste
                        END IF

                        CALL ValidaBusquedaCuenta(desc_deleg, fecha_proceso, num_issste, usuario,num_pension)
                            RETURNING query

                        IF query="" OR query IS NULL THEN
                            
                        ELSE
                            CALL list1.clear()  # Limpiar la lista
                            CALL buscarCuenta(query,list1,cve_deleg,ldateFecFinCal,ldateFecIniCal)    # Buscar datos con los criterios capturados
                        END IF

                    ON ACTION cerrar
                        --LET band=0
                        EXIT PROGRAM
                        EXIT INPUT
                END INPUT
  {} 
		

                              
			FOR I=1 TO list1.getlength()
				IF list1[I].seleccion <>list1[I].reg_enviado THEN
					LET list1[I].seleccion=list1[I].reg_enviado
				END IF
			END FOR
			LET lsi_while = TRUE
			
			CALL fgl_setkeylabel_GEN("unselectAll","Limpiar Todas")      
			CALL fgl_setkeylabel_GEN("selectAll","Aceptar Todas")
			CALL fgl_setkeylabel_GEN("Accept","Guardar")      
			--CALL fgl_setkeylabel_GEN("Cancel","Salir")
			WHILE lsi_while    
				INPUT  ARRAY list1 FROM  Record1.* 
						 ATTRIBUTES (UNBUFFERED, WITHOUT DEFAULTS, APPEND ROW = FALSE, 
						 DELETE ROW= FALSE, INSERT ROW = FALSE,CANCEL=FALSE)
					BEFORE INPUT 
						CALL UI.interface.refresh()
					ON ACTION SelectAll 
						FOR I =1 TO list1.getlength()
							LET list1[i].seleccion = list1[i].reg_enviado
						END FOR
						CALL UI.interface.refresh()
						EXIT INPUT
                        ---12 de enero de 2009 SGPC Se elimino el boton para cancelar autorizaciones de concesiones de DT.
					ON ACTION unselectAll 
						FOR I =1 TO list1.getlength()
							LET list1[i].seleccion = 0
						END FOR
						CALL UI.interface.refresh()
						EXIT INPUT
                     
                     
					ON ACTION ACCEPT 
						CALL popup("Estas seguro que deseas guardar los datos? ")RETURNING lsm_guar
						IF lsm_guar THEN
                          BEGIN WORK
							FOR iterador = 1 TO list1.getlength()
							   --IF seleccion = 1 THEN 
                                    --IF list1[iterador].seleccion <> 2 THEN
                                       DISPLAY list1[iterador].id_cuenta_sir,list1[iterador].seleccion,list1[iterador].spe_id,list1[iterador].num_issste,list1[iterador].num_pension
                                       DISPLAY "CALL ACTUALIZA ",list1[iterador].id_cuenta_sir,list1[iterador].seleccion,list1[iterador].spe_id,list1[iterador].num_issste,list1[iterador].num_pension
                                       CALL ActualizaEstatusCuenta(list1[iterador].id_cuenta_sir,list1[iterador].seleccion,list1[iterador].spe_id,list1[iterador].num_issste,list1[iterador].num_pension ) RETURNING lsm_actual
                                       DISPLAY "SE VA **************** ",lsm_actual
                                       --LET lsm_actual = TRUE
                                       IF lsm_actual THEN
                                          LET li_suma = li_suma + 1
                                       ELSE 
                                          EXIT FOR	
                                       END IF
									--END IF
                               --END IF--LET lsi_while=FALSE
                            END FOR
							--CALL FGL_WINMESSAGE( "Información","Se actualizaron  "||li_suma CLIPPED||" datos de  "||list1.getlength() CLIPPED||" con exito", "information")
							IF li_suma=list1.getlength() THEN 
							   COMMIT WORK
								 CALL FGL_WINMESSAGE( "Información","Los datos se almacenaron con exito ", "information")
                 LET lsi_while=FALSE
								-- EXIT INPUT	
							ELSE
								 ROLLBACK WORK
								 CALL FGL_WINMESSAGE( "Información","Existio un error al guardar ", "information")
							END IF
						ELSE
							LET lsi_while=FALSE
							EXIT INPUT
						END IF
						 
					AFTER INPUT
						CALL popup("Estas seguro que deseas guardar los datos? ")RETURNING lsm_guar
						IF lsm_guar THEN
							FOR iterador = 1 TO list1.getlength()
							   --IF seleccion = 1 THEN 
                                    --IF list1[iterador].seleccion <> 2 THEN
                                       DISPLAY list1[iterador].id_cuenta_sir,list1[iterador].seleccion,list1[iterador].spe_id,list1[iterador].num_issste,list1[iterador].num_pension
                                       DISPLAY "CALL ACTUALIZA ",list1[iterador].id_cuenta_sir,list1[iterador].seleccion,list1[iterador].spe_id,list1[iterador].num_issste,list1[iterador].num_pension
                                       CALL ActualizaEstatusCuenta(list1[iterador].id_cuenta_sir,list1[iterador].seleccion,list1[iterador].spe_id,list1[iterador].num_issste,list1[iterador].num_pension ) RETURNING lsm_actual
                                       DISPLAY "SE VA **************** ",lsm_actual
                                       --LET lsm_actual = TRUE
                                       IF lsm_actual THEN
                                          LET li_suma = li_suma + 1
                                       ELSE 
                                          EXIT FOR	
                                       END IF
									--END IF
                               --END IF--LET lsi_while=FALSE
                            END FOR
							--CALL FGL_WINMESSAGE( "Información","Se actualizaron  "||li_suma CLIPPED||" datos de  "||list1.getlength() CLIPPED||" con exito", "information")
							IF li_suma=list1.getlength() THEN 
							   COMMIT WORK
								 CALL FGL_WINMESSAGE( "Información","Los datos se almacenaron con exito ", "information")
                 LET lsi_while=FALSE
								 EXIT INPUT	
							ELSE
								 ROLLBACK WORK
								 CALL FGL_WINMESSAGE( "Información","Existio un error al guardar ", "information")
							END IF
						ELSE
							LET lsi_while=FALSE
							EXIT INPUT
						END IF
						
						#lipio los que no tengan 1
                        --IF cve_perfil = 'I' THEN
                        --CALL funcion_btn()
                            ON ACTION Generar
                            IF cve_perfil <> 'I' THEN
                                CALL FGL_WINMESSAGE( "ALERT", "Usted no cuenta con los permisos necesarios para generar el LAYAUT","STOP")
                            ELSE 
                                CALL Layout_cuenta_sir(ldateFecFinCal,ldateFecIniCal)
                            END IF
                            
						--END IF	
                        
                      ON ACTION cerrar
                        --LET band=0
                        EXIT PROGRAM
                        EXIT INPUT
                    
						CALL popup("Estas seguro que deseas salir sin guardar? ")RETURNING lsm_guar
						IF lsm_guar THEN
							LET lsi_while=FALSE
							EXIT INPUT
						END IF	
				END INPUT
			END WHILE 
            CALL fgl_setkeylabel_GEN("unselectAll","Limpiar Todas")      
			CALL fgl_setkeylabel_GEN("selectAll","Aceptar Todas")
            CALL fgl_setkeylabel_GEN("Guardar","Accept")      
			--CALL fgl_setkeylabel_GEN("Salir","Cancel")
        CLOSE WINDOW frmConsultaCuenta
        
END FUNCTION

FUNCTION ActualizaEstatusCuenta(id_cuenta_sir,seleccion,spe_id,num_issste,num_pension ) 
    DEFINE 
        id_cuenta_sir       INTEGER,
        spe_id           	INTEGER,
        num_tarjeta       	VARCHAR(20),
        clabe            	VARCHAR(18),
        desc_deleg         	VARCHAR(100),
        num_pension      	INTEGER,
        num_issste          INTEGER,
        neto_pagar  	DECIMAL(12,2),
        fecha_proceso    	DATE,
        reg_enviado      	INTEGER,
        fecha_enviado    	DATE,
        reg_transferido  	INTEGER,
        fecha_transferido	DATE,
        reg_cancelado    	INTEGER,
        fecha_cancelado  	DATE,
        usuar          	CHAR(8),
        fecha_aud        	DATE,
        hora_aud         	CHAR(8),
        componente_cve   	CHAR(8),
        ip_maquina       	CHAR(15),
        seleccion      	    INTEGER


  
   DEFINE ls_qry_dt STRING
   DEFINE lsm_ban SMALLINT
   DEFINE ok SMALLINT
   

DISPLAY "id     ",id_cuenta_sir,"  Seleccion     ",seleccion,"  spe_id        ",spe_id,"  numero de issste    ",num_issste,"  numero de pension    ",num_pension

   IF seleccion IS NULL THEN
     LET seleccion = 1
   END IF

   
   LET ls_qry_dt="UPDATE cuenta_sir SET reg_enviado = ", seleccion ," WHERE id_cuenta_sir =",id_cuenta_sir," AND spe_id = ",spe_id
   DISPLAY ls_qry_dt
   PREPARE s_dt FROM ls_qry_dt
   LET lsm_ban = TRUE
   --BEGIN WORK
      EXECUTE s_dt
      {IF SQLCA.SQLCODE <> 0 THEN
         LET lsm_ban = FALSE
         ROLLBACK WORK
      END IF
      --CALL bitacora_DT(usuario.cve_operador,ls_folio,li_num_issste,li_selecciona+3) RETURNING ok  # Agregado Cuéllar 8 Octubre 2009
      IF NOT ok THEN
         LET lsm_ban = FALSE
         ROLLBACK WORK
      ELSE
         COMMIT WORK
      END IF}
   RETURN lsm_ban	
END FUNCTION

# *************************************************************************************************
# Nombre_funcion		ValidaBusquedaCuenta
# Nombre del equipo		
# Desarrollado_por		
# Fecha 			    13/11/2008
# Ultima_modificacion	
# Descripción			Validar los campos, para generar la condición de búsqueda
#                       y la delegación que lo procesa
# Entra                 
#                       
#                       
#                       
#                       
#                       
#
# Salida                query : Condición de búsqueda
# **************************************************************************************************
FUNCTION ValidaBusquedaCuenta(desc_deleg,fecha_proceso, num_issste, usuario, num_pension)
    DEFINE 
        id_cuenta_sir1       INTEGER,
        spe_id1           	INTEGER,
        num_tarjeta1       	VARCHAR(20),
        clabe1            	VARCHAR(18),
        desc_deleg         	VARCHAR(100),
        num_pension      	INTEGER,
        num_issste          INTEGER,
        neto_pagar1  	DECIMAL(12,2),
        fecha_proceso    	DATE,
        reg_enviado1      	INTEGER,
        fecha_enviado1    	DATE,
        reg_transferido1  	INTEGER,
        fecha_transferido1	DATE,
        reg_cancelado1    	INTEGER,
        fecha_cancelado1  	DATE,
        usuario          	CHAR(8),
        usuari          	CHAR(8),
        fecha_aud1        	DATE,
        hora_aud1         	CHAR(8),
        componente_cve1   	CHAR(8),
        ip_maquina1       	CHAR(15),
        seleccion,li_suma      	    INTEGER
        
    DEFINE num_issste1 DECIMAL(11,0),
        rfc VARCHAR(13),
        curp VARCHAR (18),
        nombre VARCHAR(40),
        apellido_paterno VARCHAR(40),
        apellido_materno VARCHAR(40)
    DEFINE query STRING
    
    LET query="" # incializar el query
    
    DISPLAY desc_deleg,fecha_proceso, num_issste, usuario, num_pension
    --Validación del numero de Issste 
    IF num_issste>0 THEN
        LET query = "  AND cs.num_issste="||num_issste
    END IF

    IF num_pension>0 THEN
        LET query = "  AND cs.num_pension="||num_pension
    END IF

     IF fecha_proceso>0 THEN
        LET query = "  AND cs.fecha_proceso= TO_DATE('"||fecha_proceso||"','%d/%m/%Y')"
    END IF

   IF desc_deleg IS NOT NULL THEN
    --Let usuari = TRIM(usuario)
  DISPLAY usuari  
        LET query=" AND cd.desc_deleg LIKE  '"||desc_deleg CLIPPED||"%'"
    END IF  
    
    IF usuario IS NOT NULL THEN
    --Let usuari = TRIM(usuario)
  DISPLAY usuari  
        LET query=" AND cs.usuario LIKE  '"||usuario CLIPPED||"%'"
    END IF
        
    
    RETURN query
END FUNCTION

FUNCTION buscarCuenta(query,list1,cve_deleg,ldateFecFinCal,ldateFecIniCal)
    DEFINE query, query_aux STRING
    DEFINE cve_deleg SMALLINT 
    DEFINE ldateFecFinCal DATE
    DEFINE ldateFecIniCal DATE 
    DEFINE list1 DYNAMIC ARRAY OF RECORD
        id_cuenta_sir       INTEGER,
        spe_id           	INTEGER,
        num_tarjeta       	VARCHAR(20),
        clabe            	VARCHAR(18),
        desc_deleg         	VARCHAR(100),
        num_pension      	INTEGER,
        num_issste          INTEGER,
        neto_pagar   	DECIMAL(12,2),
        fecha_proceso    	DATE,
        reg_enviado      	INTEGER,
        --fecha_enviado    	DATE,
        --reg_transferido  	INTEGER,
        --fecha_transferido	DATE,
        --reg_cancelado    	INTEGER,
        --fecha_cancelado  	DATE,
        usuario          	CHAR(8),
        seleccion      	    INTEGER
        --fecha_aud        	DATE,
        --hora_aud         	CHAR(8),
        --componente_cve   	CHAR(8),
        --ip_maquina       	CHAR(15)
END RECORD
DEFINE list11 RECORD
        id_cuenta_sir       INTEGER,
        spe_id           	INTEGER,
        num_tarjeta       	VARCHAR(20),
        clabe            	VARCHAR(18),
        desc_deleg         	VARCHAR(100),
        num_pension      	INTEGER,
        num_issste          INTEGER,
        neto_pagar          DECIMAL(12,2),
        fecha_proceso    	DATE,
        reg_enviado      	INTEGER,
        --fecha_enviado    	DATE,
        --reg_transferido  	INTEGER,
        --fecha_transferido	DATE,
        --reg_cancelado    	INTEGER,
        --fecha_cancelado  	DATE,
        usuario          	CHAR(8),
        seleccion      	    INTEGER
        --fecha_aud        	DATE,
        --hora_aud         	CHAR(8),
        --componente_cve   	CHAR(8),
        --ip_maquina       	CHAR(15)
END RECORD
    DEFINE datos_reg RECORD 
                 num_isste INTEGER,
                 rfc VARCHAR(13),
                 curp VARCHAR(18),
                 nombre VARCHAR(40),
                 apellido_paterno VARCHAR(40),
                 apellido_materno VARCHAR(40)
                 END RECORD
                 
    DEFINE ind RECORD LIKE indirecto.*
    DEFINE  num_reg,
            cont,
            cont_ind, 
            num_issste INTEGER
    IF cve_deleg = 40 THEN 
    DISPLAY "delegacion 40"
        LET query_aux = "SELECT cs.id_cuenta_sir,
                                cs.spe_id,
                                cs.num_tarjeta,
                                cs.clabe,
                                cd.desc_deleg,
                                cs.num_pension,
                                cs.num_issste,
                                cs.neto_pagar,
                                cs.fecha_proceso,
                                cs.reg_enviado,
                                cs.usuario
                         FROM cuenta_sir cs, c_deleg cd
                         WHERE fecha_enviado is null
                         AND cs.dis_cve = cd.cve_deleg
                         AND fecha_proceso BETWEEN to_date('", ldateFecIniCal, "','%d/%m/%Y') 
                         AND to_date('",ldateFecFinCal,"','%d/%m/%Y')"
    ELSE
   DISPLAY "delegacion diferente a 40" 
        LET query_aux = "SELECT cs.id_cuenta_sir,
                                cs.spe_id,
                                cs.num_tarjeta,
                                cs.clabe,
                                cd.desc_deleg,
                                cs.num_pension,
                                cs.num_issste,
                                cs.neto_pagar,
                                cs.fecha_proceso,
                                cs.reg_enviado,
                                cs.usuario
                         FROM cuenta_sir cs, c_deleg cd
                         WHERE fecha_enviado is null
                         AND cs.dis_cve = cd.cve_deleg
                         AND cs.dis_cve = ", cve_deleg," 
                         AND fecha_proceso BETWEEN to_date('", ldateFecIniCal, "','%d/%m/%Y') 
                         AND to_date('",ldateFecFinCal,"','%d/%m/%Y')"
    END IF
    LET query_aux = query_aux , query
    DISPLAY query_aux
    SET ISOLATION TO DIRTY READ --Add 24/Marzo/2010
    PREPARE list_pen FROM query_aux
    DECLARE cursor_Cuenta CURSOR FOR list_pen
           
    {FOREACH cur_pen INTO list1[cont].* 
        IF cont=100 THEN
            EXIT FOREACH
        END IF
        LET cont=cont+1
    END FOREACH}
     FOREACH cursor_Cuenta INTO list11.*
     DISPLAY "dentro del foreach"
        --CALL list1.appendElement()
        --LET list1[list1.getLength()].* = list11.*
        --LET cont=cont+1
        DISPLAY list11.*
        CALL list1.appendElement()
        LET list1[list1.getLength()].* = list11.*
        END FOREACH
    SET ISOLATION TO COMMITTED READ --Add 24/Marzo/2010
    CALL list1.deleteelement(cont)
END FUNCTION



FUNCTION Layout_cuenta_sir(ldateFecFinCal,ldateFecIniCal)

    DEFINE dispersion DYNAMIC ARRAY OF RECORD
        lchrRFC CHAR(13),
        lvintNumPen INTEGER,
        lchrCodDeu CHAR(3),
        lchrNombSol CHAR(34),
        lchrEntCve CHAR(2),
        lintlocId INTEGER,
        lvDecTotPerc DECIMAL(12,2),
        lvDecTotDeduc DECIMAL(12,2),
        lvDecTotal DECIMAL(12,2),
        num_cuenta CHAR(20),
        lchrclavBanc CHAR(1),
        lchrFechaNac CHAR(8),
        lchrFechaIniPen CHAR(8),
        clabe VARCHAR(18),
        spe_id INTEGER
    END RECORD
    
    DEFINE lsqlCuentaSir STRING
    DEFINE lstrLayoutCuenta STRING  
    DEFINE ldateFecFinCal DATE 
    DEFINE ldateFecIniCal DATE 
    DEFINE i INTEGER 
    DEFINE lintTamDis INTEGER 
    DEFINE lstrNomArchivo STRING 
    DEFINE bccuenta base.Channel
    DEFINE lstrUpdCuentaSir STRING
    DEFINE lvintIdArchivo INTEGER 
    
            LET lsqlCuentaSir="select fecha_ini_semana  FROM c_calendar_sir where fecha_fin_semana=?"    
            PREPARE prelsqlCuentaSir1 FROM lsqlCuentaSir
            EXECUTE prelsqlCuentaSir1 USING ldateFecFinCal INTO ldateFecIniCal
            DISPLAY "ldateFecIniCal",ldateFecIniCal
            IF (ldateFecIniCal IS NULL OR ldateFecIniCal <='31/12/1899' ) THEN 
                CALL FGL_WINMESSAGE("Generación de Nómina","Esa fecha no existe en el calendario de Pagos","INFORMATION") 
            ELSE 
                LET bccuenta = base.Channel.create()
                LET i=1
                LET lstrLayoutCuenta = "SELECT rfc_pen, num_pension, iden_deudo, nombre, ent_pago, zon_pago, total_percep, total_deduc, 
                                        neto_pagar, num_tarjeta, clave_tarjeta, fecha_nacimiento, fecha_otorgamiento, clabe ,spe_id
                                        FROM cuenta_sir WHERE fecha_enviado is null and reg_enviado=1 and fecha_proceso BETWEEN to_date('", ldateFecIniCal, "','%d/%m/%Y') AND to_date('",ldateFecFinCal,"','%d/%m/%Y')"
                DISPLAY "lstrLayoutCuenta-->",lstrLayoutCuenta
                PREPARE prelstrLayoutCuenta FROM lstrLayoutCuenta
                DECLARE curprelstrLayoutCuenta CURSOR FOR prelstrLayoutCuenta                        

                FOREACH curprelstrLayoutCuenta INTO dispersion[i].lchrRFC,dispersion[i].lvintNumPen,dispersion[i].lchrCodDeu,dispersion[i].lchrNombSol,dispersion[i].lchrEntCve,
                                                    dispersion[i].lintlocId,dispersion[i].lvDecTotPerc,dispersion[i].lvDecTotDeduc,dispersion[i].lvDecTotal,dispersion[i].num_cuenta,
                                                    dispersion[i].lchrclavBanc,dispersion[i].lchrFechaNac,dispersion[i].lchrFechaIniPen,dispersion[i].clabe,dispersion[i].spe_id
                        LET i = i +1
                END FOREACH

                CALL dispersion.deleteelement(i);
                LET i = i - 1

                IF dispersion.getlength() IS NOT NULL AND dispersion.getlength()>0 THEN
                    
                    LET lstrNomArchivo   = "Casan_Nomina_PP_",year(ldateFecFinCal)||completar_cadena_layout("0",month(ldateFecFinCal),2)||completar_cadena_layout("0",day(ldateFecFinCal),2),".txt"
                    LET lintTamDis = dispersion.getlength()
                    CALL bccuenta.openFile(lstrNomArchivo,"w")
                    DISPLAY "lstrNomArchivo",lstrNomArchivo
                    CALL folio_cuenta_sir(lstrNomArchivo) RETURNING lvintIdArchivo
                    DISPLAY "lvintIdArchivo",lvintIdArchivo
                    FOR i=1 TO lintTamDis
                        CALL bccuenta.writeline(Formato_Cuenta(dispersion[i].*)) # Guardar en al archivo con el formato de la cadena sin delimitador
                        LET lstrUpdCuentaSir='update cuenta_sir set fecha_enviado=today,fecha_nomina=?,id_archivo=? where spe_id=?'
                        PREPARE prelstrUpdCuentaSir FROM lstrUpdCuentaSir
                        EXECUTE prelstrUpdCuentaSir USING ldateFecFinCal,lvintIdArchivo,dispersion[i].spe_id
                        FREE prelstrUpdCuentaSir
                    END FOR 
                    CALL bccuenta.CLOSE()
                    CALL SaveToFileClient(lstrNomArchivo, ".txt", "Archivo de Texto")
                ELSE 
                    CALL FGL_WINMESSAGE("Generación de Nómina","No se encontraron registros","INFORMATION") 
                  
                END IF 
            END IF 
            
END FUNCTION 

FUNCTION Formato_Cuenta(dispersion)
    DEFINE dispersion RECORD 
        lchrRFC CHAR(13),
        lvintNumPen INTEGER,
        lchrCodDeu CHAR(3),
        lchrNombSol CHAR(34),
        lchrEntCve CHAR(2),
        lintlocId INTEGER,
        lvDecTotPerc DECIMAL(12,2),
        lvDecTotDeduc DECIMAL(12,2),
        lvDecTotal DECIMAL(12,2),
        num_cuenta CHAR(20),
        lchrclavBanc CHAR(1),
        lchrFechaNac CHAR(8),
        lchrFechaIniPen CHAR(8),
        clabe VARCHAR(18),
        spe_id INTEGER
    END RECORD
  
    DEFINE renglon STRING
    
        # 1 RFC
    LET renglon = dispersion.lchrRFC
    
        # 2 Numero de pension 
    LET renglon = renglon || completar_cadena_layout("0",dispersion.lvintNumPen,7)
    
        # 3 Codido de deudo
    LET renglon = renglon || dispersion.lchrCodDeu
        
        # 4 Nombre 
    LET renglon = renglon || completar_cadena_layout(" ",dispersion.lchrNombSol,34)
        
        # 5 Entidad de pago
    LET renglon = renglon || completar_cadena_layout("0",dispersion.lchrEntCve,2)
        
        # 6 Localidad de pago
    LET renglon = renglon || completar_cadena_layout("0",dispersion.lintlocId,7)
        
        # 7 Total percepcion
    LET renglon = renglon || completar_cadena_layout("0",dispersion.lvDecTotPerc,9)
        
        # 8 Total Deduccion
    LET renglon = renglon || completar_cadena_layout("0",dispersion.lvDecTotDeduc,9)
        
        # 9 Neto a pagar
    LET renglon = renglon || completar_cadena_layout("0",dispersion.lvDecTotal,9)
        
        # 10 Numero de tarjeta
    LET renglon = renglon ||  dispersion.num_cuenta
        
        # 11 Clave tarjeta
    LET renglon = renglon ||  dispersion.lchrclavBanc
        
        # 12 Fecha de nacimiento
    LET renglon = renglon ||  dispersion.lchrFechaNac
        
        # 13 Fecha otorgamiento
    LET renglon = renglon ||  dispersion.lchrFechaIniPen
        
        # 14 Cuenta clabe
    LET renglon = renglon ||  dispersion.clabe
        
    RETURN renglon   
END FUNCTION

FUNCTION completar_cadena_layout(relleno,str,longitud)
    DEFINE relleno VARCHAR(2)
    DEFINE str,str1 STRING
    DEFINE longitud,i SMALLINT
    
    LET str1=str
    IF str<>"" OR str IS NOT NULL THEN
        LET str=str.trim()
        LET longitud = longitud - str.getlength()
    END IF
    IF str.getlength()=0 THEN
        LET str = relleno
        LET longitud = longitud - 1
    END IF
    
    IF relleno = "0" THEN
        FOR i=1 TO longitud
            LET str = relleno || str
        END FOR
    ELSE 
        FOR i=1 TO longitud
            LET str = str || " "
        END FOR
    END IF
    IF str1 IS NULL THEN
        LET str1=" "
    END IF
    
    RETURN str
END FUNCTION

FUNCTION SaveToFileClient(fileName, ext, tipoExt)
   DEFINE fileName String
   DEFINE ext      String
   DEFINE tipoExt  String
   DEFINE Cadena   String
   DEFINE flag     SmallInt

   LET flag = FALSE
   WHILE flag = FALSE
      CALL WINSAVEFILE(fileName, tipoExt, ext, "Guarda el archivo" ) RETURNING Cadena
      DISPLAY "La Cadena es: ", Cadena
      
      IF Cadena IS NOT NULL THEN
         WHENEVER ERROR CONTINUE
         CALL fgl_putfile(fileName, Cadena)
         WHENEVER ERROR STOP
      END IF
    
      IF STATUS = -8066 THEN
         CALL FGL_WINMESSAGE ("Error al guardar archivo", "Seleccione otro directorio", "stop")
      ELSE
         IF Cadena IS NOT NULL  THEN
            CALL FGL_WINMESSAGE ("Aviso", "El archivo se guardo en: " || Cadena, "information")
         ELSE
            CALL FGL_WINMESSAGE ("Aviso", "guardar cancelado", "information")
         END IF
         LET flag = TRUE
      END IF
   END WHILE
END FUNCTION

##*******************************************************************************************##
##Función que inserta el registro en folio_cta_sir del archivo generado                      ##
##Fecha: marzo 2015                                                                          ##
##Desarrollado por: Fabrica de software UCOL                                                 ##   
##*******************************************************************************************##

FUNCTION folio_cuenta_sir (lvarNombreArchivo)

DEFINE lvarNombreArchivo VARCHAR(30)
DEFINE ldteFecCreacion DATE 
DEFINE ldteFechaAud DATE 
DEFINE lstrFolCtaSir STRING 
DEFINE lchrIpMaquina CHAR(15)
DEFINE lcharUsuario CHAR(8)
DEFINE lcharComponente CHAR(8)
DEFINE lchrHoraAud CHAR(8)
DEFINE lintIdArchivo INTEGER

-- Se asignan los campos auditores para la tabla
LET ldteFecCreacion = TODAY
LET lcharUsuario = ARG_VAL(5)
LET ldteFechaAud = TODAY 
LET lchrHoraAud	= TIME
LET lcharComponente='CtaSir'
LET lchrIpMaquina  = FGL_GETENV("FGLSERVER")

-- Se prepara el query para insertar el registro en folio_cta_sir
LET lstrFolCtaSir = "INSERT INTO folio_cta_sir(id_folio, nombre_archivo, fecha_creacion, usuario, fecha_aud, hora_aud, componente_cve, ip_maquina) 
                      VALUES(0,?,?,?,?,?,?,?)"
                      
--Se ejecuta el query para insetar el registo en foli_cta_sir
PREPARE prelstrFolCtaSir FROM lstrFolCtaSir
EXECUTE prelstrFolCtaSir USING lvarNombreArchivo,ldteFecCreacion,lcharUsuario,ldteFechaAud,lchrHoraAud,lcharComponente,lchrIpMaquina
LET lintIdArchivo = SQLCA.SQLERRD[2]   
FREE prelstrFolCtaSir

--Se retorna el folio que se inserto
RETURN lintIdArchivo

END FUNCTION 