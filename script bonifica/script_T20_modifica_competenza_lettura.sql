/*
richiesta:               modificare la competenza lettura
campo di riferimento: meterhistory.accessibility
valori:               "I" per distributore, "A" per SdL
decodifica lato SAP:  "002" per distributore, "001" per SdL
famiglia ticket:      T20 
gestione service now:  risolto
*/

DECLARE
   type pdrarray IS VARRAY(250) OF VARCHAR2(14);
   -- lista pdr da verificare che richiedono la stessa competenza di lettura corretta
   pdr pdrarray;
   v_acc_act varchar(5);
   v_acc_mod integer;
   total integer;
   exist integer;
   -- competenza di lettura corretta
   v_comp_lett varchar(5);

BEGIN 
  
  pdr := pdrarray('01023900000260', '01023900004445', '01023900008974', '01023900009014', '01023900009261', '01023900012075', '01023900013156', 
'01023900013560', '01023900013594', '01023900022116', '01023900022371', '01023900024989', '01023900026984', '01023900029392', 
'01023900035803', '01023900043419', '01023900043435', '01023900043450', '01023900047816', '01023900060348', '01023900070263', 
'01023900071912', '01023900075062', '01023900075071', '01023900075486', '01023900076716', '01023900077201', '01023900080197', 
'01023900080262', '01023900080288', '01023900083084', '01023900086675', '01023900087718', '01023900088209', '01023900090170', 
'01023900093638', '01023900093653', '01023910000542', '01023910000824', '01023910000964', '01023910013260', '01023910014408', 
'01023910015434', '01023910015938', '01023910016500', '01023910016701', '01025700000994', '01025700005183', '01025700007668', 
'01025700008757', '01025700009235', '01025700012916');

  
  total :=  pdr.count;
  v_comp_lett := 'A';
  v_acc_mod := 0;


  FOR i IN 1 .. total LOOP 
       
       SELECT COUNT(1) INTO exist 
       FROM FACILITY 
       WHERE FACILITYID = pdr(i);
       
       IF(exist = 0) THEN 
        DBMS_OUTPUT.PUT_LINE('pdr: '||pdr(i)||' inesistente');
        CONTINUE;
       END IF;
       
       select mh.accessibility into v_acc_act
       from meterhistory mh,
            facility fa,
            meter me
       where mh.uidfacility = fa.uidfacility
         and me.uidmeter = mh.uidmeter
         and fa.facilityid = pdr(i) 
         and mh.stoptime is null;
          
       IF(v_acc_act = v_comp_lett) THEN 
        DBMS_OUTPUT.PUT_LINE('pdr: ' || pdr(i) || ' competenza lettura già corretta');
       else
        update meterhistory 
        set  accessibility = v_comp_lett,
		     reader = 'I'
        where uidmeterhistory = (select mh.uidmeterhistory
                                 from meterhistory mh,
                                      facility fa,
                                      meter me
                                 where mh.uidfacility = fa.uidfacility
                                   and me.uidmeter = mh.uidmeter
                                   and fa.facilityid = pdr(i)
                                   and mh.accessibility != v_comp_lett
                                   and mh.stoptime is null);
         if SQL%ROWCOUNT = 0 then
            DBMS_OUTPUT.PUT_LINE('pdr: ' || pdr(i) || ' impossibile modificare competenza lettura'); 
         else 
            DBMS_OUTPUT.PUT_LINE('pdr: ' || pdr(i) || ' competenza lettura modificata'); 
			v_acc_mod := v_acc_mod +1;
         end if;                          
                                  
       END IF; 

 END LOOP;
   DBMS_OUTPUT.PUT_LINE('pdr analizzati: '||total||' modificati: '||v_acc_mod);
 --commit;
 END; 
 
