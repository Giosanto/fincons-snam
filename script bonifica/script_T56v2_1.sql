-- questo script non avrebbe potuto essere mai e poi mai realizzato senza l'ausilio di Davide Cancellara



DECLARE 

type v_list                       IS VARRAY(30000) OF VARCHAR2 (14);
type s_list                        IS VARRAY(30000) OF NUMBER;
type d_list                        IS VARRAY(30000) OF DATE;
v_pdr                             VARCHAR2 (14);
v_kcoefftype                       VARCHAR2 (20);
v_mistype                        meter.TYPE%TYPE;
v_juriscode                     jurisdiction.juriscode%TYPE;
v_juris_altitude                jurisdiction.altitude%TYPE;
v_gastype                        plant.gastype%TYPE;
v_inizio_fatt                    DATE;
v_date_cmd                        DATE;
v_ck                                FLOAT(52);
v_fh_ko                            BOOLEAN;
v_dbms_tmp                        VARCHAR2 (30000);
v_date_ko                        DATE;
v_cr_causal                        VARCHAR2 (120 BYTE); 
v_data_limite_storno             DATE := SYSDATE - (365 * 5);
v_inizio_fatt_opcoco            DATE := TO_DATE ('01072003', 'ddmmyyyy');
simulazione                        BOOLEAN;
v_second                        NUMBER;
v_new_supp_press                NUMBER;
nro_serv_com                     NUMBER;
nro_serv_tec                     NUMBER;
nro_pdr                            NUMBER;
v_uid                                 NUMBER;
new_ktype                       facilityhistory.uidkcoefftype%TYPE;
new_kval                         facilityhistory.kcoeffval%TYPE;
v_corr                            NUMBER;
v_date_all_null					BOOLEAN;

type mr_rec_typ IS RECORD (        stopreadtime_tcn     meterread.stopreadtime%TYPE,
                                stopreadtime_com     meterread.stopreadtime%TYPE,
                                meterstatusrsncode   meterstatusrsn.meterstatusrsncode%TYPE,
                                psbltrans            meterstatusrsn.psbltrans%TYPE,              -- per migliore gestione DROP (v.38)
                                transactionno        meterread.transactionno%TYPE,
                                is_default           NUMBER, -- 0 FALSE, 1 TRUE
                                drop_comm            NUMBER);-- 0 FALSE, 1 TRUE   
type mr_tab_typ                 IS TABLE OF mr_rec_typ INDEX BY BINARY_INTEGER;                                                             
mr_rec                          mr_rec_typ;
mr_tab                          mr_tab_typ;
mr_tab_empty                    mr_tab_typ; 

CURSOR c_fh (c_uid NUMBER) IS 
   SELECT  fh.*     
   FROM facilityhistory fh            
   WHERE fh.uidfacility = c_uid; 

v_pdr_list v_list;
v_press_list s_list;
v_date_list d_list;

 PROCEDURE check_date_ko (i_date DATE, p_cr_causal VARCHAR, v_forza_limite_storno BOOLEAN DEFAULT FALSE) IS
    BEGIN
        IF  NVL (v_date_ko, SYSDATE) > i_date
        AND (i_date >= v_inizio_fatt OR v_forza_limite_storno)                        -- (v.41) per permettere di iniziare lo storno prima dell'inizo della fau
        AND (i_date >= NVL (v_data_limite_storno, TO_DATE ('01/01/2000', 'dd/mm/yyyy')) OR v_forza_limite_storno) THEN                                -- (v.37)
             v_date_ko      := TRUNC (i_date);
             v_cr_causal    := p_cr_causal;
        END IF;
    END check_date_ko;
  
---------------------------------------------------------------------------------------------------------------------------------------------------------------  
    
    FUNCTION get_data_inizio_fatt (f_uidfacility NUMBER)
      RETURN DATE IS
      f_inizio_fatt   DATE;
      f_inizio_enrl   DATE;
    BEGIN
    -- estraggo data inizio fatturazione
        BEGIN
             SELECT MIN (ID.startreadtime)
               INTO f_inizio_fatt
               FROM invoice i, invoicedetail ID, billingadjustment ba
              WHERE i.uidinvoice = ID.uidinvoice AND i.invoiceid LIKE '%DV%' AND ID.uidfacility = f_uidfacility AND ID.uidba = ba.uidba
                    AND UPPER (ba.bacode) = 'NORMAL';

             --  DBMS_OUTPUT.put_line ('F ' || f_inizio_fatt);
             SELECT NVL (MIN (ROUND (mr.stopreadtime)), TO_DATE ('01011900', 'ddmmyyyy'))
               INTO f_inizio_enrl
               FROM meterread mr, meterhistory mh
              WHERE mh.uidfacility = f_uidfacility
                AND mh.uidmeter = mr.uidmeter
                AND mh.status = 'O'
                AND ROUND (mr.stopreadtime) >= mh.starttime
                AND mr.uidmeterreadstatus IN (RM_PKG_GLOBAL_VAR.uid_used, RM_PKG_GLOBAL_VAR.uid_usable)
                AND mr.uidmeterstatusrsn = RM_PKG_GLOBAL_VAR.v_enrl;

             --   DBMS_OUTPUT.put_line ('E ' || f_inizio_enrl);
            IF f_inizio_enrl >= v_inizio_fatt_opcoco AND f_inizio_enrl < NVL (f_inizio_fatt, SYSDATE) THEN
                f_inizio_fatt    := f_inizio_enrl;
                v_date_ko        := f_inizio_fatt;
                v_cr_causal      := 'PQD_NEG_ERR';
            END IF;

            IF f_inizio_fatt IS NULL THEN
                BEGIN
                         SELECT MIN (starttime)
                         INTO f_inizio_fatt
                         FROM facilityannusage
                         WHERE uidfacility = f_uidfacility;

                    IF f_inizio_fatt IS NULL THEN
                            -- prima lettura
                          SELECT NVL (MIN (ROUND (mr.stopreadtime)), TO_DATE ('01011900', 'ddmmyyyy'))
                            INTO f_inizio_enrl
                            FROM meterread mr, meterhistory mh
                            WHERE mh.uidfacility = f_uidfacility
                             AND mh.uidmeter = mr.uidmeter
                             AND mh.status = 'O'
                             AND ROUND (mr.stopreadtime) >= mh.starttime
                             AND mr.uidmeterreadstatus IN (RM_PKG_GLOBAL_VAR.uid_used, RM_PKG_GLOBAL_VAR.uid_usable);

                              IF f_inizio_enrl >= v_inizio_fatt_opcoco AND f_inizio_enrl < NVL (f_inizio_fatt, SYSDATE) THEN
                                 f_inizio_fatt    := f_inizio_enrl;
                                 v_date_ko        := f_inizio_fatt;
                                 v_cr_causal      := 'PQD_NEG_ERR';
                              ELSE
                                 f_inizio_fatt    := v_inizio_fatt_opcoco;
                              END IF;
                    END IF;
                END;
            END IF;
        END;

        RETURN f_inizio_fatt;
    END get_data_inizio_fatt;





BEGIN 

        simulazione := TRUE;
        v_pdr_list := v_list('00880000321987');
        v_press_list := s_list('3500');
		v_date_list := d_list(null);
		v_date_all_null := TRUE;  --TRUE sse per tutti i pdr non è specificata una data, FALSE altrimenti.
        nro_pdr := v_pdr_list.count;
        
		IF(v_date_all_null) THEN
			IF (v_pdr_list.count <> v_press_list.count) OR (v_date_list.count <> v_press_list.count) THEN 
				raise_application_error(-20101, 'Numero di pdr diverso dal numero dei valori pressione');
			END IF;
        ELSE 
			IF (v_pdr_list.count <> v_press_list.count) OR (v_date_list.count <> v_press_list.count) OR (v_pdr_list.count <> v_date_list.count) THEN 
				raise_application_error(-20101, 'Numero di pdr diverso dal numero dei valori pressione o dal numero di date');
			END IF;
		END IF;
		
        FOR i IN 1 .. nro_pdr LOOP
				
                BEGIN
                SELECT UIDFACILITY INTO v_uid FROM FACILITY WHERE FACILITYID = v_pdr_list(i);
                EXCEPTION 
                WHEN NO_DATA_FOUND THEN 
                    v_uid := null;
                END;
               
                IF(v_uid IS NULL)THEN 
                     
                    DBMS_OUTPUT.PUT_LINE(v_pdr_list(i) || ' inesistente');
                   
                   select COUNT(*) into nro_serv_com
                   from asi_richiesta@ASI_PUBBL_LETTURA_CNT2004 where pdr =  v_pdr_list(i) and STATO = 'ETC';
                    IF(nro_serv_com = 0) THEN 
                       DBMS_OUTPUT.PUT_LINE('per il pdr ' || v_pdr_list(i) || ' non ci sono servizi commerciali in ETC');
                    ELSE 
                       DBMS_OUTPUT.PUT_LINE('per il pdr ' || v_pdr_list(i) || '  ci sono servizi commerciali in ETC');
                    END IF; 
                    
                    select COUNT(*) into nro_serv_tec
                    from asi_pronto_intervento@ASI_PUBBL_LETTURA_CNT2004  where pdr = v_pdr_list(i) and STATO = 'ETC';
                    IF(nro_serv_tec = 0) THEN 
                       DBMS_OUTPUT.PUT_LINE('per il pdr ' || v_pdr_list(i) || ' non ci sono servizi tecnici in ETC');
                    ELSE 
                       DBMS_OUTPUT.PUT_LINE('per il pdr ' || v_pdr_list(i) || '  ci sono servizi tecnici in ETC');
                    END IF;   
                    CONTINUE;    
                    
                ELSE  
               
                      --DBMS_OUTPUT.PUT_LINE(v_pdr_list(i) || ' esiste');
                      FOR r IN c_fh(v_uid) LOOP
						
						IF(v_date_all_null) THEN
							IF (v_press_list(i) = r.supplypressure) THEN 
								DBMS_OUTPUT.PUT_LINE('per uidfacilityhistory ' || r.uidfacilityhistory || ' e pdr ' || v_pdr_list(i) 
                                                ||' non è necessario aggiornare la pressione in data ' || r.starttime);
								CONTINUE;
							END IF;	
						ELSE			
							IF (v_press_list(i) = r.supplypressure) OR r.starttime < NVL(v_date_list(i),TO_DATE ('31/12/1099', 'dd/mm/yyyy')) THEN 
								DBMS_OUTPUT.PUT_LINE('per uidfacilityhistory ' || r.uidfacilityhistory || ' e pdr ' || v_pdr_list(i) 
												||' non è necessario aggiornare la pressione in data ' || r.starttime);
								CONTINUE;
							END IF;
                        END IF;                      
                        
                        UPDATE FACILITYHISTORY
                        SET SUPPLYPRESSURE = v_press_list(i)
                        WHERE UIDFACILITYHISTORY = r.uidfacilityhistory;
                        DBMS_OUTPUT.PUT_LINE('_____ ' ||r.uidfacilityhistory || 'update fatto e ' || 'nuova pressione: ' || v_press_list(i)  );
                        r.supplypressure := v_press_list(i);
                        v_inizio_fatt        := get_data_inizio_fatt(r.uidfacility);
                        BEGIN
                            select kcoeffval, uidkcoefftype 
                            into new_kval,new_ktype
                            from facilityhistory
                            where uidfacilityhistory = r.uidfacilityhistory;
                            DBMS_OUTPUT.PUT_LINE('vecchio kcoeffval: ' || new_kval || ' ,vecchio uidkcoefftype: ' || new_ktype);
                        END;
                        BEGIN
                            select kcoefftype into v_kcoefftype from kcoefftype where uidkcoefftype =  r.uidkcoefftype; 
                            DBMS_OUTPUT.PUT_LINE('tipo k: ' || v_kcoefftype);
                        EXCEPTION
                        WHEN NO_DATA_FOUND THEN  
                               v_kcoefftype := null; 
                        END;
                        v_date_cmd            := NULL;
                        v_fh_ko               := FALSE;
                        BEGIN 
                            SELECT COUNT (mh.corrmeterid)
                            into v_corr
                            from facilityhistory fh, meterhistory mh 
                            where fh.uidfacility = v_uid
                            and mh.uidfacility = fh.uidfacility 
                            and fh.uidfacilityhistory = r.uidfacilityhistory
                             and mh.starttime <= r.starttime and NVL(mh.stoptime,TO_DATE ('31/12/2099', 'dd/mm/yyyy')) > r.starttime; 
                            EXCEPTION
                            WHEN NO_DATA_FOUND THEN 
                                    v_corr := 0;
                        END;
                         DBMS_OUTPUT.PUT_LINE('count corr: ' || v_corr);

                        IF v_corr > 0 THEN
                            DBMS_OUTPUT.PUT_LINE('per' || r.uidfacilityhistory || 'corrmeterid non è null');
                            IF v_kcoefftype IS NULL OR v_kcoefftype NOT LIKE 'CORRECTIVE' THEN
                                DBMS_OUTPUT.PUT_LINE('Update KcoeffType per presenza correttore ' || r.stoptime);

                                UPDATE facilityhistory
                                SET uidkcoefftype = (SELECT uidkcoefftype
                                                     FROM kcoefftype
                                                     WHERE kcoefftype = 'CORRECTIVE'),                                                                --(v.36)
                                kcoeffval = NVL (kcoeffval, 1)
                                WHERE uidfacilityhistory = r.uidfacilityhistory;

                                check_date_ko (r.starttime, 'PRESS_ERR');
                            END IF;
                        ELSIF v_kcoefftype LIKE 'CORRECTIVE' THEN
                            DBMS_OUTPUT.PUT_LINE('Update KcoeffType per assenza correttore ma CORRECTIVE ' || r.stoptime);

                            UPDATE facilityhistory                                                                                                     -- (v.38)
                            SET uidkcoefftype = NULL,
                            kcoeffval = NULL
                            WHERE uidfacilityhistory = r.uidfacilityhistory;

                            v_kcoefftype    := NULL;
                        END IF;

                        -- verifico il K contrattuale, in assenza di K correttivo
                        -- possibile fare il controllo solo dopo la delibera 159/08, prima di questa data la logica prevedeva pressione superiore a 25
                        -- ma entrava in gioco anche il calibro

                                                -- aggiunto campo METER.TYPE data modifica alla procedura calcola_ck (v.40)
                        /* modificato per corretta gestione nuovi tipi misuratore (v.42)
                        S  Standard
                        I  Integrato
                        T  Corretto in temperatura
                        P  Corretto in pressione
                        Se non è standard, il misuratore deve sempre avere un K contrattuale
                        */
                        IF v_inizio_fatt > r.starttime THEN
                            v_date_cmd    := v_inizio_fatt;
                        ELSE
                            v_date_cmd    := r.starttime;
                        END IF;

                        BEGIN
                            SELECT m.TYPE
                            INTO v_mistype
                            FROM meter m INNER JOIN meterhistory mh
                            ON m.uidmeter = mh.uidmeter
                            AND mh.uidfacility = r.uidfacility
                            AND v_date_cmd BETWEEN mh.starttime AND NVL (mh.stoptime, SYSDATE + 1)
                            AND mh.status IN ('O');
                            DBMS_OUTPUT.PUT_LINE(v_mistype || ' tipo misuratore');
                            EXCEPTION
                            WHEN NO_DATA_FOUND THEN
                                 BEGIN
                                SELECT m.TYPE
                                INTO v_mistype
                                FROM meter m INNER JOIN meterhistory mh
                                    ON m.uidmeter = mh.uidmeter
                                    AND mh.uidfacility = r.uidfacility
                                    AND v_date_cmd BETWEEN mh.starttime AND NVL (mh.stoptime, SYSDATE + 1)
                                    AND mh.status IN ('C');
                                    EXCEPTION
                                    WHEN NO_DATA_FOUND THEN
                                        v_mistype := 'S';
                                END;   
                        END;

                        IF (v_kcoefftype LIKE 'CONTRACTUAL' OR v_kcoefftype IS NULL) AND is_in_159_08 (NULL, NVL (r.stoptime, SYSDATE)) THEN
                            DBMS_OUTPUT.put_line ('rientra nella delibera');
                            SELECT DISTINCT p.gastype
                                       INTO v_gastype
                                       FROM locality l, plant p
                                      WHERE l.uidlocality = r.uidlocality AND l.uidplant = p.uidplant;
                                      DBMS_OUTPUT.PUT_LINE(v_gastype || ' tipo gas');
                            IF (v_gastype = 'M' AND r.supplypressure > get_lsparameter_float ('BOUND_METANO_FOR_CK'))
                                OR (v_gastype = 'G' AND r.supplypressure > get_lsparameter_float ('BOUND_GPL_FOR_CK'))
                                OR v_mistype <> 'S' THEN
                                -- corretta chiamata calcola_ck (v.45)
                                    SELECT DISTINCT j.juriscode, j.altitude
                                    INTO v_juriscode, v_juris_altitude
                                    FROM jurisdiction j, locality l, facilityhistory fh
                                    WHERE fh.uidfacility = r.uidfacility AND l.uidlocality = fh.uidlocality AND j.juriscode = l.juriscode; 
                                    
                                    v_ck    := calcola_ck (v_mistype, r.supplypressure, v_juriscode, r.uidlocality, v_juris_altitude, r.real_altitude);

                                    --DBMS_OUTPUT.put_line ('V_CK ' || to_char(v_ck));
                                    IF v_ck IS NULL AND (r.uidkcoefftype IS NOT NULL OR r.kcoeffval IS NOT NULL) THEN
                                        
                                        IF v_mistype = 'S' THEN
                                            UPDATE facilityhistory
                                            SET uidkcoefftype = NULL,
                                                kcoeffval = NULL
                                            WHERE uidfacilityhistory = r.uidfacilityhistory;

                                            check_date_ko (r.starttime, 'PRESS_ERR');
                                            v_fh_ko       := TRUE;
                                            DBMS_OUTPUT.PUT_LINE('Update K contrattuale: NULL. Tipo Misuratore ' || v_mistype || ' - Stoptime' || r.stoptime);
                                        END IF;
                                    ELSIF v_ck IS NOT NULL AND (r.uidkcoefftype IS NULL OR r.kcoeffval <> v_ck) THEN
                                    --DBMS_OUTPUT.put_line ('entrato!!!!!!!!!!!');
                                                UPDATE facilityhistory
                                                SET uidkcoefftype = (SELECT uidkcoefftype
                                                                     FROM kcoefftype
                                                                     WHERE kcoefftype = 'CONTRACTUAL'),
                                                    kcoeffval = v_ck
                                                WHERE uidfacilityhistory = r.uidfacilityhistory;

                                                check_date_ko (r.starttime, 'PRESS_ERR');
                                                v_fh_ko       := TRUE;
                                                DBMS_OUTPUT.PUT_LINE('Update K contrattuale: ' || v_ck || ' Tipo Misuratore ' || v_mistype || ' - Stoptime '|| r.stoptime);
                                    END IF;
                            ELSE
                                IF v_kcoefftype LIKE 'CONTRACTUAL' THEN
                                    -- Se misuratore standard e non raggiunge la soglia di pressione, tolgo il K contrattuale (v.42)
                                    UPDATE facilityhistory
                                    SET uidkcoefftype = NULL,
                                        kcoeffval = NULL
                                    WHERE uidfacilityhistory = r.uidfacilityhistory;

                                    check_date_ko (r.starttime, 'PRESS_ERR');
                                    v_fh_ko       := TRUE;
                                    DBMS_OUTPUT.PUT_LINE('Update K contrattuale: NULL. Tipo Misuratore ' || v_mistype || ' - Stoptime'|| r.stoptime);
                                END IF;
                            END IF;
                        END IF;

                        IF r.kcoeffval IS NULL AND v_kcoefftype IS NOT NULL THEN  -- e non ho appena cancellato il correttore nell'if sopra, metto 1 nel kcoeffval
                                        
                            DBMS_OUTPUT.PUT_LINE('Update KcoeffType per assenza kcoeffval ' || r.stoptime);

                            UPDATE facilityhistory     -- (v.38)
                            SET kcoeffval = 1    -- valore di default
                            WHERE uidfacilityhistory = r.uidfacilityhistory;
                            
                        END IF;
         
                        BEGIN
                            select kcoeffval, uidkcoefftype 
                            into new_kval,new_ktype
                            from facilityhistory
                            where uidfacilityhistory = r.uidfacilityhistory;
                            DBMS_OUTPUT.PUT_LINE('nuovo kcoeffval: ' || new_kval || ' ,nuovo uidkcoefftype: ' || new_ktype);
                        END;
                    END LOOP;
                END IF;    
        END LOOP;
        
        IF c_fh%ISOPEN THEN
            CLOSE c_fh;
        END IF;
        
        IF(simulazione) THEN 
            ROLLBACK;
            DBMS_OUTPUT.PUT_LINE('rollback ');
        ELSE 
            COMMIT;
        END IF;
        
END;

--Se is_in_159_08='Y' allora:
--    Se Misuratore=STANDARD allora:
--          Se è a Metano allora:
--                Se pressione>get_lsparameter_float ('BOUND_METANO_FOR_CK') allora:
--                           KCONTRATTUALE=OK
--          Se è a GPL allora:
--                Se pressione>get_lsparameter_float ('BOUND_GPL_FOR_CK') allora:
--                           KCONTRATTUALE=OK
--     Se Misuratore corretto in pressione o temperatura allora:
--                 KCONTRATTUALE=OK
--Se is_in_159_08='N' allora:
--                Se pressione>25 + (altri parametri che sto cercando di recuperare) allora:
--                           KCONTRATTUALE=OK
--questa è la regola per i periodi prima della delibera 159-08
-- Se meterClassCode >= 40
-- calculateK
--Se meterClassCode < 40
-- v_prpres > 40 mBar -> calculateK
-- v_prpres <= 40 mBar NO CK -> va applicato M comunale return NULL
   