-- T19 - Modifica Ubicazione PDR (in realtà ubicazione misuratore)

-- Alter session set current_schema=USER_MNGR_FATITG_01
-- Alter session set current_schema=USER_MNGR_FATAES_01
-- Alter session set current_schema=USER_MNGR_FATTOS_01
-- Alter session set current_schema=USER_MNGR_FATNAP_01
-- Alter session set current_schema=USER_MNGR_FATUDG_01
-- Alter session set current_schema=USER_MNGR_FATANG_01

DECLARE
   -- INIZIO -- dati da modificare
   l_pdr           facility.facilityid%TYPE := '00594200432617';
   l_updrerrato    VARCHAR2 (32767) := '001';
   l_updresatto    VARCHAR2 (32767) := '010';
   l_simulazione   BOOLEAN := TRUE;
   -- FINE -- dati da modificare

   l_uidf          facility.uidfacility%TYPE;
   l_updrerr       meterloc.meterloccd%TYPE;
   l_updresa       meterloc.meterloccd%TYPE;
   l_uidmlatt      VARCHAR2 (32767);
   l_uidbiff       meterloc.uidbiffatura%TYPE;
   l_uidmlnew      VARCHAR2 (32767);
   l_uidbiffnew    meterloc.uidbiffatura%TYPE;
   l_acc229        rm_tb_biffature.acc229%TYPE;
   l_uidmeter      meterhistory.uidmeter%TYPE;
   no_pdr          EXCEPTION;
BEGIN
   -- recupera uidfacility
   BEGIN
      SELECT uidfacility
        INTO l_uidf
        FROM facility
       WHERE facilityid = l_pdr;
   EXCEPTION
      WHEN NO_DATA_FOUND
      THEN
         RAISE no_pdr;
   END;

   l_updrerrato := LTRIM (l_updrerrato, '0');
   l_updresatto := LTRIM (l_updresatto, '0');

   l_updrerr := LPAD (l_updrerrato, 3, '0');
   l_updresa := LPAD (l_updresatto, 3, '0');

   SELECT ml.meterloccd, ml.uidbiffatura
     INTO l_uidmlatt, l_uidbiff
     FROM facility f, facatthistory fah, meterloc ml
    WHERE     f.facilityid = l_pdr
          AND f.uidfacility = fah.uidfacility
          AND fah.uidmeterloc = ml.uidmeterloc
          AND fah.stoptime IS NULL;

   SELECT uidmeterloc, uidbiffatura
     INTO l_uidmlnew, l_uidbiffnew
     FROM meterloc
    WHERE meterloccd = l_updresa;

   DBMS_OUTPUT.
    PUT_LINE ('Ubicazione PdR attuale: ' || LTRIM (l_uidmlatt, '0'));


   IF LTRIM (l_uidmlatt, '0') = l_updresatto
   THEN
      DBMS_OUTPUT.
       PUT_LINE (
         'Nessuna attività effettuata. ' || 'Ubicazione PdR già corretto.');
   ELSE
      UPDATE facatthistory
         SET uidmeterloc =
                (SELECT uidmeterloc
                   FROM meterloc
                  WHERE meterloccd = l_updresa)
       WHERE uidfacility = l_uidf AND stoptime IS NULL;

      SELECT acc229
        INTO l_acc229
        FROM rm_tb_biffature
       WHERE uidbiffatura = l_uidbiffnew;
	   
	   DBMS_OUTPUT.
    PUT_LINE ('Nuova accessibilità 229: ' || l_acc229);

      SELECT uidmeter
        INTO l_uidmeter
        FROM meterhistory
       WHERE uidfacility = l_uidf AND stoptime IS NULL;

      UPDATE meterhistory
         SET accessibility229 = l_acc229
       WHERE uidfacility = l_uidf AND uidmeter = l_uidmeter;

      DBMS_OUTPUT.
       PUT_LINE (
         'Attività effettuata. Cambio ubicazione in: ' || l_updresatto);
   END IF;

   IF (l_simulazione)
   THEN
      ROLLBACK;
   ELSE
      COMMIT;
   END IF;
EXCEPTION
   WHEN no_pdr
   THEN
      DBMS_OUTPUT.PUT_LINE ('PdR ' || l_pdr || ' non presente a sistema');
END;