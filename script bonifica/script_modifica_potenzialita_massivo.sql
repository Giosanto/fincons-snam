/*
MODIFICA POTENZIALITA'

MAXIMUMHOURLYFLOW = POTENZIALITA' = POT
POTENTIALUSE= MASSIMA POTENZIALITA' = POTENZIALITA' SU SAP = POT_MAX
MAXFLOWDPREQ = POTENZIALITA' RICHIESTA = POT_RICH

NB: SETTARE A -1 I CAMPI IN CUI NON SONO PRESENTI DATI, ECCETTO LA DATA CHE E' DA SETTARE A NULL.

*/

DECLARE
   V_UID_FACATTHISTORY   FACATTHISTORY.UIDFACATTHISTORY%TYPE;
   V_SIMULAZIONE         BOOLEAN := TRUE;
   V_NRO_SERV_COM        NUMBER;
   V_NRO_SERV_TEC        NUMBER;
   

   CURSOR CURR_POT
   IS
      (SELECT '00880001395197' PDR,
              -1 POT,
              58 POT_MAX,
              -1 POT_RICH,
              NULL DATA_MOD
         FROM DUAL);


   PROCEDURE AGGIORNA_POTENZIALITA (
      P_PDR                  FACILITY.FACILITYID%TYPE,
      P_UID_FACATTHISTORY    FACATTHISTORY.UIDFACATTHISTORY%TYPE,
      P_POT                  FACATTHISTORY.MAXIMUMHOURLYFLOW%TYPE,
      P_POT_MAX              FACATTHISTORY.POTENTIALUSE%TYPE,
      P_POT_RICH             FACATTHISTORY.MAXFLOWDPREQ%TYPE,
      P_SIMULAZIONE          BOOLEAN)
   IS
      P_POT_OLD        FACATTHISTORY.MAXIMUMHOURLYFLOW%TYPE;
      P_POT_MAX_OLD    FACATTHISTORY.POTENTIALUSE%TYPE;
      P_POT_RICH_OLD   FACATTHISTORY.MAXFLOWDPREQ%TYPE;
   BEGIN
      IF (P_POT_MAX >= 0 AND P_POT > P_POT_MAX)
      THEN
         DBMS_OUTPUT.PUT_LINE (
               'PDR : '
            || P_PDR
            || ' - LA POTENZIALITA'' NON PUO'' ESSERE MAGGIORE DELLA POTENZIALITA'' MASSIMA. CONTROLLARE DATI FORNITI DA UTENTE');
      ELSE
         SELECT FAH.MAXIMUMHOURLYFLOW, FAH.POTENTIALUSE, FAH.MAXFLOWDPREQ
           INTO P_POT_OLD, P_POT_MAX_OLD, P_POT_RICH_OLD
           FROM FACATTHISTORY FAH
          WHERE FAH.UIDFACATTHISTORY = P_UID_FACATTHISTORY;

         IF (P_POT >= 0)
         THEN                                   -- AGGIORNAMENTO POTENZIALITA'
            IF (P_POT = P_POT_OLD)
            THEN
               DBMS_OUTPUT.PUT_LINE (
                     'PDR : '
                  || P_PDR
                  || ' - LA POTENZIALITA'' INDICATA E'' GIA'' PRESENTE A SISTEMA');
            ELSE
               IF (P_SIMULAZIONE)
               THEN
                  DBMS_OUTPUT.PUT_LINE (
                        'PDR : '
                     || P_PDR
                     || 'UIDFACATTHISTORY : '
                     || P_UID_FACATTHISTORY
                     || ' - AGGIORNARE LA POTENZIALITA'' DA '
                     || P_POT_OLD
                     || ' A '
                     || P_POT);
               ELSE
                  UPDATE FACATTHISTORY FAH
                     SET FAH.MAXIMUMHOURLYFLOW = P_POT,
                         FAH.POTENTIALUSE =
                            CASE
                               WHEN     P_POT > FAH.POTENTIALUSE
                                    AND P_POT_MAX < 0
                               THEN
                                  P_POT
                               ELSE
                                  FAH.POTENTIALUSE
                            END
                   WHERE FAH.UIDFACATTHISTORY = P_UID_FACATTHISTORY;
               END IF;
            END IF;
         END IF;



         IF (P_POT_MAX >= 0)
         THEN                           -- AGGIORNAMENTO POTENZIALITA' MASSIMA
            IF (P_POT_MAX = P_POT_MAX_OLD)
            THEN
               DBMS_OUTPUT.PUT_LINE (
                     'PDR : '
                  || P_PDR
                  || ' - LA POTENZIALITA'' MASSIMA INDICATA E'' GIA'' PRESENTE A SISTEMA');
            ELSE
               IF (P_SIMULAZIONE)
               THEN
                  DBMS_OUTPUT.PUT_LINE (
                        'PDR : '
                     || P_PDR
                     || 'UIDFACATTHISTORY : '
                     || P_UID_FACATTHISTORY
                     || ' - AGGIORNARE LA POTENZIALITA'' MASSIMA DA '
                     || P_POT_MAX_OLD
                     || ' A '
                     || P_POT_MAX);
               ELSE
                  UPDATE FACATTHISTORY FAH
                     SET FAH.POTENTIALUSE = P_POT_MAX,
                         FAH.MAXIMUMHOURLYFLOW =
                            CASE
                               WHEN P_POT_MAX < FAH.MAXIMUMHOURLYFLOW
                               THEN
                                  P_POT_MAX
                               ELSE
                                  FAH.MAXIMUMHOURLYFLOW
                            END
                   WHERE FAH.UIDFACATTHISTORY = P_UID_FACATTHISTORY;
               END IF;
            END IF;
         END IF;



         IF (P_POT_RICH >= 0)
         THEN                         -- AGGIORNAMENTO POTENZIALITA' RICHIESTA
            IF (P_POT_RICH = P_POT_RICH_OLD)
            THEN
               DBMS_OUTPUT.PUT_LINE (
                     'PDR : '
                  || P_PDR
                  || ' - LA POTENZIALITA'' RICHIESTA INDICATA E'' GIA'' PRESENTE A SISTEMA');
            ELSE
               IF (P_SIMULAZIONE)
               THEN
                  DBMS_OUTPUT.PUT_LINE (
                        'PDR : '
                     || P_PDR
                     || 'UIDFACATTHISTORY : '
                     || P_UID_FACATTHISTORY
                     || ' - AGGIORNARE LA POTENZIALITA'' RICHIESTA DA '
                     || P_POT_RICH_OLD
                     || ' A '
                     || P_POT_RICH);
               ELSE
                  UPDATE FACATTHISTORY FAH
                     SET FAH.MAXFLOWDPREQ = P_POT_RICH
                   WHERE FAH.UIDFACATTHISTORY = P_UID_FACATTHISTORY;
               END IF;
            END IF;
         END IF;
      END IF;
   END;

BEGIN
   FOR CURR_POT_ROW IN CURR_POT
   LOOP
      BEGIN
         IF (GET_UIDFACILITY (CURR_POT_ROW.PDR) IS NULL)
         THEN
            DBMS_OUTPUT.PUT_LINE (
               'PDR ' || CURR_POT_ROW.PDR || ' NON PRESENTE A SISTEMA');
		 
         SELECT COUNT(*) INTO V_NRO_SERV_COM
            FROM ASI_RICHIESTA@ASI_PUBBL_LETTURA_CNT2004 WHERE PDR = CURR_POT_ROW.PDR AND STATO = 'ETC';
            IF(V_NRO_SERV_COM = 0) THEN 
               DBMS_OUTPUT.PUT_LINE('per il pdr ' || CURR_POT_ROW.PDR || ' non ci sono servizi commerciali in ETC');
            ELSE 
               DBMS_OUTPUT.PUT_LINE('per il pdr ' || CURR_POT_ROW.PDR || ' ci sono servizi commerciali in ETC');
            END IF; 
            
         SELECT COUNT(*) INTO V_NRO_SERV_TEC
            FROM ASI_PRONTO_INTERVENTO@ASI_PUBBL_LETTURA_CNT2004  where PDR = CURR_POT_ROW.PDR and STATO = 'ETC';
            IF(V_NRO_SERV_COM = 0) THEN 
               DBMS_OUTPUT.PUT_LINE('per il pdr ' || CURR_POT_ROW.PDR || ' non ci sono servizi tecnici in ETC');
            ELSE 
               DBMS_OUTPUT.PUT_LINE('per il pdr ' || CURR_POT_ROW.PDR || '  ci sono servizi tecnici in ETC');
            END IF;   		
			
         ELSE
            IF (CURR_POT_ROW.DATA_MOD IS NULL)
            THEN
               SELECT FAH.UIDFACATTHISTORY
                 INTO V_UID_FACATTHISTORY
                 FROM FACATTHISTORY FAH
                WHERE     FAH.UIDFACILITY =
                             GET_UIDFACILITY (CURR_POT_ROW.PDR)
                      AND FAH.STARTTIME =
                             (SELECT MAX (FHA2.STARTTIME)
                                FROM FACATTHISTORY FHA2
                               WHERE FHA2.UIDFACILITY = FAH.UIDFACILITY);

               AGGIORNA_POTENZIALITA (CURR_POT_ROW.PDR,
                                      V_UID_FACATTHISTORY,
                                      CURR_POT_ROW.POT,
                                      CURR_POT_ROW.POT_MAX,
                                      CURR_POT_ROW.POT_RICH,
                                      V_SIMULAZIONE);
            ELSE
               FOR FAH_ROW
                  IN (SELECT FAH.UIDFACATTHISTORY
                        FROM FACATTHISTORY FAH
                       WHERE     FAH.UIDFACILITY =
                                    GET_UIDFACILITY (CURR_POT_ROW.PDR)
                             AND (   FAH.STOPTIME IS NULL
                                  OR FAH.STOPTIME >= CURR_POT_ROW.DATA_MOD))
               LOOP
                  AGGIORNA_POTENZIALITA (CURR_POT_ROW.PDR,
                                         FAH_ROW.UIDFACATTHISTORY,
                                         CURR_POT_ROW.POT,
                                         CURR_POT_ROW.POT_MAX,
                                         CURR_POT_ROW.POT_RICH,
                                         V_SIMULAZIONE);
               END LOOP;
            END IF;
         END IF;
      END;
   END LOOP;
END;