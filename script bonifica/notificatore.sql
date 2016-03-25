-- Notificatore (SOLO ENROLLMENT)

-- ALTER SESSION SET CURRENT_SCHEMA=USER_MNGR_FATITG_01
-- ALTER SESSION SET CURRENT_SCHEMA=USER_MNGR_FATAES_01
-- ALTER SESSION SET CURRENT_SCHEMA=USER_MNGR_FATTOS_01
-- ALTER SESSION SET CURRENT_SCHEMA=USER_MNGR_FATNAP_01
-- ALTER SESSION SET CURRENT_SCHEMA=USER_MNGR_FATUDG_01
-- ALTER SESSION SET CURRENT_SCHEMA=USER_MNGR_FATANG_01

DECLARE
   -- INIZIO -- DATI DA MODIFICARE
   L_PDR            FACILITY.FACILITYID%TYPE := '00352800225238';
   L_TRANSID        METERREAD.TRANSACTIONNO%TYPE := '20160311152317760431';
   L_SIMULAZIONE    BOOLEAN := TRUE;
   -- FINE -- DATI DA MODIFICARE

   L_BON            BOOLEAN := FALSE;
   L_UIDF           FACILITY.UIDFACILITY%TYPE;
   L_METER          METER.UIDMETER%TYPE;
   L_UIDREAD        METERREAD.UIDREAD%TYPE;
   L_DATACOMP       METERREAD.STOPREADTIME%TYPE;
   L_NOMESERV       METERSTATUSRSN.METERSTATUSRSNCODE%TYPE;
   L_LOCLETT        VARCHAR2 (50);
   L_LETTSUCC       METERREAD.UIDREAD%TYPE;
   L_PRATICA        VARCHAR2 (50);

   NO_PDR           EXCEPTION;
   NO_TRANSIDINMR   EXCEPTION;
   NO_METER         EXCEPTION;
   NO_PRATICA       EXCEPTION;
   TOO_SERV         EXCEPTION;
   NO_MR            EXCEPTION;
   NO_FAU           EXCEPTION;
   NO_FH            EXCEPTION;
   NO_FAH           EXCEPTION;
   NO_MH            EXCEPTION;
   NO_IDSM          EXCEPTION;
   NO_LTMHEI        EXCEPTION;
   NO_ZTMP          EXCEPTION;
   NO_GEST          EXCEPTION;
BEGIN
   -- RECUPERA UIDFACILITY
   BEGIN
      SELECT UIDFACILITY
        INTO L_UIDF
        FROM FACILITY
       WHERE FACILITYID = L_PDR;
   EXCEPTION
      WHEN NO_DATA_FOUND
      THEN
         RAISE NO_PDR;
   END;

   -- RECUPERA UIDREAD, DATA DI COMPETENZA, E NOME SERVIZIO ASSOCIATI AL
   -- NUMERO DI TRANSAZIONE
   BEGIN
      BEGIN
         SELECT MH.UIDMETER
           INTO L_METER
           FROM METERHISTORY MH
          WHERE MH.UIDFACILITY = L_UIDF AND MH.STOPTIME IS NULL;
      EXCEPTION
         WHEN NO_DATA_FOUND
         THEN
            RAISE NO_METER;
      END;

      SELECT UIDREAD,
             TRUNC (STOPREADTIME),
             METERSTATUSRSNCODE,
             LOCATION
        INTO L_UIDREAD,
             L_DATACOMP,
             L_NOMESERV,
             L_LOCLETT
        FROM (SELECT MR.UIDREAD,
                     MR.STOPREADTIME,
                     RSN.METERSTATUSRSNCODE,
                     'MR' AS LOCATION
                FROM    METERREAD MR
                     JOIN
                        METERSTATUSRSN RSN
                     ON MR.UIDMETERSTATUSRSN = RSN.UIDMETERSTATUSRSN
               WHERE MR.UIDMETER = L_METER AND MR.TRANSACTIONNO = L_TRANSID
              UNION
              SELECT MRQ.UIDREAD,
                     MRQ.STOPREADTIME,
                     RSN.METERSTATUSRSNCODE,
                     'MRQ' AS LOCATION
                FROM    METERREADQUEUE MRQ
                     JOIN
                        METERSTATUSRSN RSN
                     ON MRQ.UIDMETERSTATUSRSN = RSN.UIDMETERSTATUSRSN
               WHERE MRQ.UIDMETER = L_METER AND MRQ.TRANSACTIONNO = L_TRANSID);
   EXCEPTION
      WHEN NO_DATA_FOUND
      THEN
         RAISE NO_TRANSIDINMR;
      WHEN NO_METER
      THEN
         RAISE NO_METER;
   END;

   -- NUMERO DI PRATICA
   BEGIN
      SELECT ACCTVALIDATOR
        INTO L_PRATICA
        FROM HUBTRANSINSTANCE
       WHERE TRANSID = L_TRANSID;
   EXCEPTION
      WHEN NO_DATA_FOUND
      THEN
         RAISE NO_PRATICA;
   END;

   -- CONTROLLA SE IL SERVIZIO E' L'ULTIMO ENTRATO A SISTEMA
   BEGIN
      IF (L_LOCLETT = 'MR')
      THEN
         SELECT MR.UIDREAD
           INTO L_LETTSUCC
           FROM METERREAD MR
          WHERE MR.UIDMETER = L_METER
                AND TRUNC (MR.STOPREADTIME) > L_DATACOMP;
      ELSE
         SELECT MRQ.UIDREAD
           INTO L_LETTSUCC
           FROM METERREADQUEUE MRQ
          WHERE MRQ.UIDMETER = L_METER
                AND TRUNC (MRQ.STOPREADTIME) > L_DATACOMP;
      END IF;
   EXCEPTION
      WHEN NO_DATA_FOUND
      THEN
         L_BON := TRUE;
      WHEN TOO_MANY_ROWS
      THEN
         L_BON := FALSE;
   END;

   BEGIN
      IF (L_BON)
      THEN
         DBMS_OUTPUT.PUT_LINE ('Bonifica fattibile');

         -- LA BONIFICA DIFFERISCE A SECONDA DEL SERVIZIO
         CASE L_NOMESERV
            WHEN 'ENRL'
            THEN
               DBMS_OUTPUT.
                PUT_LINE (
                     'Il servizio associato alla transazione '
                  || L_TRANSID
                  || ' è un servizio di Enrollment');

               -- INIZIO -- BONIFICA METERREAD
               DBMS_OUTPUT.
                PUT_LINE (
                  'Cancello il record nella METERREAD con i seguenti dati: '
                  || 'UIDREAD -> '
                  || L_UIDREAD);

               IF (L_LOCLETT = 'MR')
               THEN
                  DELETE FROM METERREAD
                        WHERE UIDREAD = L_UIDREAD;

                  IF (SQL%ROWCOUNT = 0 OR SQL%ROWCOUNT > 1)
                  THEN
                     RAISE NO_MR;
                  END IF;
               ELSE
                  DELETE FROM METERREADQUEUE
                        WHERE UIDREAD = L_UIDREAD;

                  IF (SQL%ROWCOUNT = 0 OR SQL%ROWCOUNT > 1)
                  THEN
                     RAISE NO_MR;
                  END IF;
               END IF;

               -- FINE -- BONIFICA METERREAD

               -- INIZIO -- BONIFICA FACILITYANNUSAGE
               DECLARE
                  TYPE L_FAUTAB IS TABLE OF FACILITYANNUSAGE%ROWTYPE
                                      INDEX BY PLS_INTEGER;

                  L_UIDSFAU        L_FAUTAB;
                  L_FAUTY          FACILITYANNUSAGE.TARIFFYEAR%TYPE;
                  L_UIDSFAUPRINT   VARCHAR2 (32767);
                  I                PLS_INTEGER;
               BEGIN
                  SELECT *
                    BULK COLLECT INTO L_UIDSFAU
                    FROM FACILITYANNUSAGE
                   WHERE UIDFACILITY = L_UIDF AND STARTTIME > L_DATACOMP;

                  I := L_UIDSFAU.FIRST;
                  L_UIDSFAUPRINT :=
                        'TARIFFYEAR -> '
                     || L_UIDSFAU (I).TARIFFYEAR
                     || ' | '
                     || 'UIDFACILITY -> '
                     || L_UIDF
                     || ' | '
                     || 'STARTTIME -> '
                     || L_UIDSFAU (I).STARTTIME;
                  I := L_UIDSFAU.NEXT (I);

                  WHILE I IS NOT NULL
                  LOOP
                     L_UIDSFAUPRINT :=
                           L_UIDSFAUPRINT
                        || ', '
                        || 'TARIFFYEAR -> '
                        || L_UIDSFAU (I).TARIFFYEAR
                        || ' | '
                        || 'UIDFACILITY -> '
                        || L_UIDF
                        || ' | '
                        || 'STARTTIME -> '
                        || L_UIDSFAU (I).STARTTIME;
                     I := L_UIDSFAU.NEXT (I);
                  END LOOP;

                  DBMS_OUTPUT.
                   PUT_LINE (
                'Cancello i record nella FACILITYANNUSAGE con i seguenti dati: '
                     || L_UIDSFAUPRINT);

                  I := L_UIDSFAU.FIRST;

                  WHILE I IS NOT NULL
                  LOOP
                     DELETE FROM FACILITYANNUSAGE
                           WHERE     TARIFFYEAR = L_UIDSFAU (I).TARIFFYEAR
                                 AND UIDFACILITY = L_UIDF
                                 AND STARTTIME = L_UIDSFAU (I).STARTTIME;

                     IF (SQL%ROWCOUNT = 0 OR SQL%ROWCOUNT > 1)
                     THEN
                        RAISE NO_FAU;
                     END IF;

                     I := L_UIDSFAU.NEXT (I);
                  END LOOP;
               END;

               -- FINE -- BONIFICA FACILITYANNUSAGE

               -- INIZIO -- BONIFICA FACILITYHISTORY
               DECLARE
                  L_UIDFHULT   FACILITYHISTORY.UIDFACILITYHISTORY%TYPE;
                  L_UIDFHPEN   FACILITYHISTORY.UIDFACILITYHISTORY%TYPE;
               BEGIN
                  SELECT FH.UIDFACILITYHISTORY
                    INTO L_UIDFHULT
                    FROM FACILITYHISTORY FH
                   WHERE FH.UIDFACILITY = L_UIDF
                         AND FH.STARTTIME = (SELECT MAX (STARTTIME)
                                               FROM FACILITYHISTORY
                                              WHERE UIDFACILITY = L_UIDF);

                  SELECT FH.UIDFACILITYHISTORY
                    INTO L_UIDFHPEN
                    FROM FACILITYHISTORY FH
                   WHERE FH.UIDFACILITY = L_UIDF
                         AND FH.STARTTIME =
                                (SELECT MAX (STARTTIME)
                                   FROM FACILITYHISTORY
                                  WHERE UIDFACILITY = L_UIDF
                                        AND STARTTIME <
                                               (SELECT STARTTIME
                                                  FROM FACILITYHISTORY
                                                 WHERE UIDFACILITYHISTORY =
                                                          L_UIDFHULT));

                  DBMS_OUTPUT.
                   PUT_LINE (
                'Cancello il record nella FACILITYHISTORY con i seguenti dati: '
                     || 'UIDFACILITYHISTORY -> '
                     || L_UIDFHULT);

                  DELETE FROM FACILITYHISTORY
                        WHERE UIDFACILITYHISTORY = L_UIDFHULT;

                  IF (SQL%ROWCOUNT = 0 OR SQL%ROWCOUNT > 1)
                  THEN
                     RAISE NO_FH;
                  END IF;

                  DBMS_OUTPUT.
                   PUT_LINE (
                'Modifico nella FACILITYHISTORY il record con i seguenti dati: '
                     || 'UIDFACILITYHISTORY -> '
                     || L_UIDFHPEN
                     || ' SET '
                     || 'STOPTIME = NULL');

                  UPDATE FACILITYHISTORY
                     SET STOPTIME = NULL
                   WHERE UIDFACILITYHISTORY = L_UIDFHPEN;

                  IF (SQL%ROWCOUNT = 0 OR SQL%ROWCOUNT > 1)
                  THEN
                     RAISE NO_FH;
                  END IF;
               EXCEPTION
                  WHEN NO_DATA_FOUND
                  THEN
                     RAISE NO_FH;
               END;

               -- FINE -- BONIFICA FACILITYHISTORY

               -- INIZIO -- BONIFICA FACATTHISTORY
               DECLARE
                  L_UIDFAHULT   FACATTHISTORY.UIDFACATTHISTORY%TYPE;
                  L_UIDFAHPEN   FACATTHISTORY.UIDFACATTHISTORY%TYPE;
                  L_UPDATEFAH      BOOLEAN := TRUE;
               BEGIN
                  SELECT FAH.UIDFACATTHISTORY
                    INTO L_UIDFAHULT
                    FROM FACATTHISTORY FAH
                   WHERE FAH.UIDFACILITY = L_UIDF
                         AND FAH.STARTTIME = (SELECT MAX (STARTTIME)
                                                FROM FACATTHISTORY
                                               WHERE UIDFACILITY = L_UIDF);

                  BEGIN
                  SELECT FAH.UIDFACATTHISTORY
                    INTO L_UIDFAHPEN
                    FROM FACATTHISTORY FAH
                   WHERE FAH.UIDFACILITY = L_UIDF
                         AND FAH.STARTTIME =
                                (SELECT MAX (STARTTIME)
                                   FROM FACATTHISTORY
                                  WHERE UIDFACILITY = L_UIDF
                                        AND STARTTIME <
                                               (SELECT STARTTIME
                                                  FROM FACATTHISTORY
                                                 WHERE UIDFACATTHISTORY =
                                                          L_UIDFAHULT));
                  EXCEPTION
                    WHEN NO_DATA_FOUND
                      THEN
                         L_UPDATEFAH := FALSE;
                   END;

                  -- GESTIONE TABELLE RM_TB_STG_FM E RM_TB_FACILITYMANAGER PER
                  -- QUANTO CONOSCIUTO. MODIFICHIAMO I RECORD DELLE TABELLE CHE
                  -- HANNO L'UIDFACATTHISTORY UGUALE A QUELLO DA CANCELLARE CON
                  -- L'UIDFACATTHISTORY DEL PENULTIMO RECORD ATTIVO.
                  DECLARE
                     TYPE L_UIDSTGFMTAB IS TABLE OF RM_TB_STG_FM.
                                                     UIDFACILITYMANAGER%TYPE
                                              INDEX BY PLS_INTEGER;

                     L_UIDSTGFMS     L_UIDSTGFMTAB;

                     TYPE L_UIDFMTAB IS TABLE OF RM_TB_FACILITYMANAGER.
                                                  UIDFACILITYMANAGER%TYPE
                                           INDEX BY PLS_INTEGER;

                     L_UIDFMS        L_UIDFMTAB;
                     L_UIDFMSPRINT   VARCHAR2 (32767);
                     I               PLS_INTEGER;
                  BEGIN
                     SELECT UIDFACILITYMANAGER
                       BULK COLLECT INTO L_UIDSTGFMS
                       FROM RM_TB_STG_FM
                      WHERE UIDFACATTHISTORY = L_UIDFAHULT;

                     IF (L_UIDSTGFMS.COUNT > 0)
                     THEN
                        I := L_UIDSTGFMS.FIRST;
                        L_UIDFMSPRINT := L_UIDSTGFMS (I);
                        I := L_UIDSTGFMS.NEXT (I);

                        WHILE I IS NOT NULL
                        LOOP
                           L_UIDFMSPRINT :=
                              L_UIDFMSPRINT || ', ' || L_UIDSTGFMS (I);
                           I := L_UIDSTGFMS.NEXT (I);
                        END LOOP;

                        IF (L_UPDATEFAH)
                        THEN
                            DBMS_OUTPUT.
                             PUT_LINE (
                    'Modifico nella RM_TB_STG_FM i record con i seguenti dati: '
                               || 'UIDFACILITYMANAGER -> '
                               || L_UIDFMSPRINT
                               || ' SET UIDFACATTHISTORY = '
                               || L_UIDFAHPEN);

                            I := L_UIDSTGFMS.FIRST;

                            WHILE I IS NOT NULL
                            LOOP
                               UPDATE RM_TB_STG_FM
                                  SET UIDFACATTHISTORY = L_UIDFAHPEN
                                WHERE UIDFACILITYMANAGER = L_UIDSTGFMS (I);

                               IF (SQL%ROWCOUNT = 0 OR SQL%ROWCOUNT > 1)
                               THEN
                                  RAISE NO_FAH;
                               END IF;

                               I := L_UIDSTGFMS.NEXT (I);
                            END LOOP;
                        ELSE
                            DBMS_OUTPUT.
                             PUT_LINE (
                    'Cancello nella RM_TB_STG_FM i record con i seguenti dati: '
                               || 'UIDFACILITYMANAGER -> '
                               || L_UIDFMSPRINT);

                            I := L_UIDSTGFMS.FIRST;

                            WHILE I IS NOT NULL
                            LOOP
                               DELETE RM_TB_STG_FM
                                WHERE UIDFACILITYMANAGER = L_UIDSTGFMS (I);

                               IF (SQL%ROWCOUNT = 0 OR SQL%ROWCOUNT > 1)
                               THEN
                                  RAISE NO_FAH;
                               END IF;

                               I := L_UIDSTGFMS.NEXT (I);
                            END LOOP;
                        END IF;
                     END IF;

                     SELECT UIDFACILITYMANAGER
                       BULK COLLECT INTO L_UIDFMS
                       FROM RM_TB_FACILITYMANAGER
                      WHERE UIDFACATTHISTORY = L_UIDFAHULT;

                     IF (L_UIDFMS.COUNT > 0)
                     THEN
                        I := L_UIDFMS.FIRST;
                        L_UIDFMSPRINT := L_UIDFMS (I);
                        I := L_UIDFMS.NEXT (I);

                        WHILE I IS NOT NULL
                        LOOP
                           L_UIDFMSPRINT :=
                              L_UIDFMSPRINT || ', ' || L_UIDFMS (I);
                           I := L_UIDFMS.NEXT (I);
                        END LOOP;

                        IF (L_UPDATEFAH)
                        THEN
                            DBMS_OUTPUT.
                             PUT_LINE (
           'Modifico nella RM_TB_FACILITYMANAGER i record con i seguenti dati: '
                               || 'UIDFACILITYMANAGER -> '
                               || L_UIDFMSPRINT
                               || ' SET UIDFACATTHISTORY = '
                               || L_UIDFAHPEN);

                            I := L_UIDFMS.FIRST;

                            WHILE I IS NOT NULL
                            LOOP
                               UPDATE RM_TB_FACILITYMANAGER
                                  SET UIDFACATTHISTORY = L_UIDFAHPEN
                                WHERE UIDFACILITYMANAGER = L_UIDFMS (I);

                               IF (SQL%ROWCOUNT = 0 OR SQL%ROWCOUNT > 1)
                               THEN
                                  RAISE NO_FAH;
                               END IF;

                               I := L_UIDFMS.NEXT (I);
                            END LOOP;
                        ELSE
                            DBMS_OUTPUT.
                             PUT_LINE (
           'Cancello nella RM_TB_FACILITYMANAGER i record con i seguenti dati: '
                               || 'UIDFACILITYMANAGER -> '
                               || L_UIDFMSPRINT);

                            I := L_UIDFMS.FIRST;

                            WHILE I IS NOT NULL
                            LOOP
                               DELETE RM_TB_FACILITYMANAGER
                                WHERE UIDFACILITYMANAGER = L_UIDFMS (I);

                               IF (SQL%ROWCOUNT = 0 OR SQL%ROWCOUNT > 1)
                               THEN
                                  RAISE NO_FAH;
                               END IF;

                               I := L_UIDFMS.NEXT (I);
                            END LOOP;
                        END IF;
                     END IF;
                  END;

                  DBMS_OUTPUT.
                   PUT_LINE (
                  'Cancello il record nella FACATTHISTORY con i seguenti dati: '
                     || 'UIDFACATTHISTORY -> '
                     || L_UIDFAHULT);

                  DELETE FROM FACATTHISTORY
                        WHERE UIDFACATTHISTORY = L_UIDFAHULT;

                  IF (SQL%ROWCOUNT = 0 OR SQL%ROWCOUNT > 1)
                  THEN
                     RAISE NO_FAH;
                  END IF;

                  IF (L_UPDATEFAH)
                  THEN
                      DBMS_OUTPUT.
                       PUT_LINE (
                  'Modifico nella FACATTHISTORY il record con i seguenti dati: '
                         || 'UIDFACATTHISTORY -> '
                         || L_UIDFAHPEN
                         || ' SET '
                         || 'STOPTIME = NULL');

                      UPDATE FACATTHISTORY
                         SET STOPTIME = NULL
                       WHERE UIDFACATTHISTORY = L_UIDFAHPEN;

                      IF (SQL%ROWCOUNT = 0 OR SQL%ROWCOUNT > 1)
                      THEN
                         RAISE NO_FAH;
                      END IF;
                  END IF;
               EXCEPTION
                  WHEN NO_DATA_FOUND
                  THEN
                     RAISE NO_FAH;
               END;

               -- FINE -- BONIFICA FACATTHISTORY

               -- INIZIO -- BONIFICA METERHISTORY
               DECLARE
                  TYPE L_UIDMHTAB IS TABLE OF METERHISTORY.UIDMETERHISTORY%TYPE
                                        INDEX BY PLS_INTEGER;

                  L_UIDMHS        L_UIDMHTAB;
                  L_UIDMH         METERHISTORY.UIDMETERHISTORY%TYPE;
                  L_DATAMH        VARCHAR2 (32767)
                     := TO_CHAR (L_DATACOMP, 'DD/MM/YYYY') || ' 12:00:00';
                  L_DATAMHMOD     VARCHAR2 (32767)
                     := TO_CHAR (L_DATACOMP, 'DD/MM/YYYY') || ' 11:00:00';
                  L_UIDMHSPRINT   VARCHAR2 (32767);
                  L_UPDATEMH      BOOLEAN := TRUE;
                  I               PLS_INTEGER;
               BEGIN
                  SELECT UIDMETERHISTORY
                    BULK COLLECT INTO L_UIDMHS
                    FROM METERHISTORY
                   WHERE UIDFACILITY = L_UIDF
                         AND NVL (
                                STOPTIME,
                                TO_DATE ('01/01/2099 23:59:59',
                                         'DD/MM/YYYY HH24:MI:SS')) >=
                                TO_DATE (L_DATAMH, 'DD/MM/YYYY HH24:MI:SS');

                  I := L_UIDMHS.FIRST;
                  L_UIDMHSPRINT := L_UIDMHS (I);
                  I := L_UIDMHS.NEXT (I);

                  WHILE I IS NOT NULL
                  LOOP
                     L_UIDMHSPRINT := L_UIDMHSPRINT || ', ' || L_UIDMHS (I);
                     I := L_UIDMHS.NEXT (I);
                  END LOOP;

                  DBMS_OUTPUT.
                   PUT_LINE (
                    'Cancello i record nella METERHISTORY con i seguenti dati: '
                     || 'UIDMETERHISTORY -> '
                     || L_UIDMHSPRINT);

                  I := L_UIDMHS.FIRST;

                  WHILE I IS NOT NULL
                  LOOP
                     DELETE FROM METERHISTORY
                           WHERE UIDMETERHISTORY = L_UIDMHS (I);

                     IF (SQL%ROWCOUNT = 0 OR SQL%ROWCOUNT > 1)
                     THEN
                        RAISE NO_MH;
                     END IF;

                     I := L_UIDMHS.NEXT (I);
                  END LOOP;

                  BEGIN
                     SELECT UIDMETERHISTORY
                       INTO L_UIDMH
                       FROM METERHISTORY MH
                      WHERE UIDFACILITY = L_UIDF
                            AND STOPTIME >
                                   TO_DATE (L_DATAMHMOD,
                                            'DD/MM/YYYY HH24:MI:SS')
                            AND STOPTIME <
                                   TO_DATE (L_DATAMH,
                                            'DD/MM/YYYY HH24:MI:SS');
                  EXCEPTION
                     WHEN NO_DATA_FOUND
                     THEN
                        L_UPDATEMH := FALSE;
                     WHEN TOO_MANY_ROWS
                     THEN
                        RAISE NO_MH;
                  END;

                  IF (L_UPDATEMH)
                  THEN
                     DBMS_OUTPUT.
                      PUT_LINE (
                   'Modifico nella METERHISTORY il record con i seguenti dati: '
                        || 'UIDMETERHISTORY -> '
                        || L_UIDMH
                        || ' SET STOPTIME = NULL');

                     UPDATE METERHISTORY
                        SET STOPTIME = NULL
                      WHERE UIDMETERHISTORY = L_UIDMH;

                     IF (SQL%ROWCOUNT = 0 OR SQL%ROWCOUNT > 1)
                     THEN
                        RAISE NO_MH;
                     END IF;
                  END IF;
               EXCEPTION
                  WHEN NO_MH
                  THEN
                     RAISE NO_MH;
               END;

               -- FINE -- BONIFICA METERHISTORY

               -- INIZIO -- BONIFICA INVDETAILSTAGEMTR
               DECLARE
                  L_UIDIDSM   INVDETAILSTAGEMTR.UIDINVDETSTAGEMTR%TYPE;
               BEGIN
                  SELECT UIDINVDETSTAGEMTR
                    INTO L_UIDIDSM
                    FROM INVDETAILSTAGEMTR
                   WHERE UIDFACILITY = L_UIDF AND REQUESTID = L_PRATICA;

                  DBMS_OUTPUT.
                   PUT_LINE (
              'Cancello il record nella INVDETAILSTAGEMTR con i seguenti dati: '
                     || 'UIDINVDETSTAGEMTR -> '
                     || L_UIDIDSM);

                  DELETE FROM INVDETAILSTAGEMTR
                        WHERE UIDINVDETSTAGEMTR = L_UIDIDSM;

                  IF (SQL%ROWCOUNT = 0 OR SQL%ROWCOUNT > 1)
                  THEN
                     RAISE NO_IDSM;
                  END IF;
               EXCEPTION
                  WHEN NO_DATA_FOUND
                  THEN
                     RAISE NO_IDSM;
               END;

               -- FINE -- BONIFICA INVDETAILSTAGEMTR

               -- INIZIO -- BONIFICA LTMH2_ENRL_INBOUND
               DECLARE
                  L_UIDEI   LTMH2_ENRL_INBOUND.UIDENRLINBOUND%TYPE;
               BEGIN
                  SELECT UIDENRLINBOUND
                    INTO L_UIDEI
                    FROM LTMH2_ENRL_INBOUND
                   WHERE     COD_PDR = L_PDR
                         AND NUM = L_TRANSID
                         AND PRATICA = L_PRATICA;

                  DBMS_OUTPUT.
                   PUT_LINE (
             'Cancello il record nella LTMH2_ENRL_INBOUND con i seguenti dati: '
                     || 'UIDENRLINBOUND -> '
                     || L_UIDEI);

                  DELETE FROM LTMH2_ENRL_INBOUND
                        WHERE UIDENRLINBOUND = L_UIDEI;

                  IF (SQL%ROWCOUNT = 0 OR SQL%ROWCOUNT > 1)
                  THEN
                     RAISE NO_LTMHEI;
                  END IF;
               EXCEPTION
                  WHEN NO_DATA_FOUND
                  THEN
                     RAISE NO_LTMHEI;
               END;

               -- FINE -- BONIFICA LTMH2_ENRL_INBOUND

               -- INIZIO -- BONIFICA ZTMP_TM_TB_NOTIFICATION
               DECLARE
                  L_TID   ZTMP_TM_TB_NOTIFICATION.TRANSID%TYPE;
               BEGIN
                  SELECT TRANSID
                    INTO L_TID
                    FROM ZTMP_TM_TB_NOTIFICATION
                   WHERE TRANSID = L_TRANSID;

                  DBMS_OUTPUT.
                   PUT_LINE (
        'Cancello il record nella ZTMP_TM_TB_NOTIFICATION con i seguenti dati: '
                     || 'TRANSID -> '
                     || L_TRANSID);

                  DELETE FROM ZTMP_TM_TB_NOTIFICATION
                        WHERE TRANSID = L_TRANSID;

                  IF (SQL%ROWCOUNT = 0 OR SQL%ROWCOUNT > 1)
                  THEN
                     RAISE NO_ZTMP;
                  END IF;
               EXCEPTION
                  WHEN NO_DATA_FOUND
                  THEN
                     RAISE NO_ZTMP;
               END;

               -- FINE -- BONIFICA ZTMP_TM_TB_NOTIFICATION

               -- INIZIO -- RISOTTOMISSIONE DEL SERVIZIO
               DBMS_OUTPUT.
                PUT_LINE ('Risottomissione del servizio di Enrollment');

               UPDATE HUBTASKINSTANCE
                  SET STATUSCD = 'E'
                WHERE UIDHUBTRANSINSTANCE IN (SELECT UIDHUBTRANSINSTANCE
                                                FROM HUBTRANSINSTANCE
                                               WHERE TRANSID = L_TRANSID);

               UPDATE HUBPROCINSTANCE
                  SET STATUSCD = 'R'
                WHERE UIDHUBPROCINSTANCE IN
                         (SELECT PI.UIDHUBPROCINSTANCE
                            FROM HUBPROCINSTANCE PI,
                                 HUBPROCTRANSINSTANCE PTI,
                                 HUBTRANSINSTANCE TI
                           WHERE PI.UIDHUBPROCINSTANCE =
                                    PTI.UIDHUBPROCINSTANCE
                                 AND PTI.UIDHUBTRANSINSTANCE =
                                        TI.UIDHUBTRANSINSTANCE
                                 AND TI.TRANSID IN (L_TRANSID));

               UPDATE HUBTRANSINSTANCE
                  SET STATUSCD = 'R'
                WHERE TRANSID IN (L_TRANSID);
            -- FINE -- RISOTTOMISSIONE DEL SERVIZIO
            WHEN 'VOLT'
            THEN
               DBMS_OUTPUT.
                PUT_LINE (
                     'Il servizio associato alla transazione '
                  || L_TRANSID
                  || ' è un servizio di Voltura');

               DBMS_OUTPUT.
                PUT_LINE ('Il servizio al momento non può essere gestito. 
                     Occorre una bonifica manuale');
            ELSE
               RAISE NO_GEST;
         END CASE;
      ELSE
         RAISE TOO_SERV;
      END IF;
   EXCEPTION
      WHEN NO_MR
      THEN
         RAISE NO_MR;
      WHEN NO_FAU
      THEN
         RAISE NO_FAU;
      WHEN NO_FH
      THEN
         RAISE NO_FH;
      WHEN NO_FAH
      THEN
         RAISE NO_FAH;
      WHEN NO_IDSM
      THEN
         RAISE NO_IDSM;
      WHEN NO_LTMHEI
      THEN
         RAISE NO_LTMHEI;
      WHEN NO_ZTMP
      THEN
         RAISE NO_ZTMP;
      WHEN NO_GEST
      THEN
         RAISE NO_GEST;
   END;

   IF (L_SIMULAZIONE)
   THEN
      ROLLBACK;
      DBMS_OUTPUT.PUT_LINE ('Simulazione eseguita');
   ELSE
      COMMIT;
      DBMS_OUTPUT.PUT_LINE ('Bonifica eseguita');
   END IF;
EXCEPTION
   WHEN NO_PDR
   THEN
      DBMS_OUTPUT.PUT_LINE ('PdR ' || L_PDR || ' non presente a sistema');
   WHEN NO_TRANSIDINMR
   THEN
      DBMS_OUTPUT.
       PUT_LINE (
            'Lettura associata alla transazione '
         || L_TRANSID
         || ' non presente a sistema. Occorre una bonifica manuale');
   WHEN NO_METER
   THEN
      DBMS_OUTPUT.PUT_LINE ('Nessun misuratore associato al PdR ' || L_PDR);
   WHEN NO_PRATICA
   THEN
      DBMS_OUTPUT.
       PUT_LINE (
         'Impossibile recuperare la pratica associata alla 
      transazione ' || L_TRANSID);
   WHEN TOO_SERV
   THEN
      DBMS_OUTPUT.
       PUT_LINE (
         'Il servizio associato alla transazione ' || L_TRANSID
         || ' non è l''ultimo entrato a sistema. Occorre una bonifica manuale');
   WHEN NO_MR
   THEN
      DBMS_OUTPUT.PUT_LINE ('Situazione incoerente in METERREAD');
      ROLLBACK;
   WHEN NO_FAU
   THEN
      DBMS_OUTPUT.PUT_LINE ('Situazione incoerente in FACILITYANNUSAGE');
      ROLLBACK;
   WHEN NO_FH
   THEN
      DBMS_OUTPUT.PUT_LINE ('Situazione incoerente in FACILITYHISTORY');
      ROLLBACK;
   WHEN NO_FAH
   THEN
      DBMS_OUTPUT.PUT_LINE ('Situazione incoerente in FACATTHISTORY');
      ROLLBACK;
   WHEN NO_MH
   THEN
      DBMS_OUTPUT.PUT_LINE ('Situazione incoerente in METERHISTORY');
      ROLLBACK;
   WHEN NO_IDSM
   THEN
      DBMS_OUTPUT.
       PUT_LINE (
        'Situazione incoerente in INVDETAILSTAGEMTR. Gestire INVDETAILMTRSRV.');
      ROLLBACK;
   WHEN NO_LTMHEI
   THEN
      DBMS_OUTPUT.PUT_LINE ('Situazione incoerente in LTMH2_ENRL_INBOUND');
      ROLLBACK;
   WHEN NO_ZTMP
   THEN
      DBMS_OUTPUT.
       PUT_LINE ('Situazione incoerente in ZTMP_TM_TB_NOTIFICATION');
      ROLLBACK;
   WHEN NO_GEST
   THEN
      DBMS_OUTPUT.
       PUT_LINE (
            'Il servizio associato alla transazione '
         || L_TRANSID
         || ' non può essere gestito. Occorre una bonifica manuale');
END;