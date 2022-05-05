-- Refresh procedure
CREATE OR REPLACE PROCEDURE P_RAFFRAICHIR_VM_APP_SECTION IS
BEGIN
    DBMS_MVIEW.REFRESH('VM_APP_SECTION', METHOD => 'C', ATOMIC_REFRESH => FALSE);
END;

CREATE OR REPLACE PROCEDURE P_RAFFRAICHIR_VM_APP_EX_IMP IS
BEGIN
    DBMS_MVIEW.REFRESH('VM_APP_EX_IMPUTATION', METHOD => 'C', ATOMIC_REFRESH => FALSE,PARALLELISM => 4);
END;

CREATE OR REPLACE PROCEDURE P_IMPORTER_POSTES_GC(audit_acteur IN VARCHAR2,audit_fonctionalite IN VARCHAR2,audit_action_creer IN VARCHAR2,audit_action_modifier IN VARCHAR2,audit_date IN DATE) AS
    vsql VARCHAR2(4000);
BEGIN
    vsql := 'MERGE INTO SIIBC_ACTEUR.poste p USING SIIBC_MEA.fonction_execution f ON (p.identifiant = f.fonc_id) '
    ||'WHEN MATCHED THEN '
    ||'UPDATE SET p.code = f.fonc_code,p.libelle = f.fonc_liblg,p.audit_acteur = :1,p.audit_fonctionalite = :2,p.audit_action = :3,p.audit_date = :4 '
    ||'WHERE f.fonc_type like ''G%'' OR f.fonc_type like ''A1%'' '
    ||'WHEN NOT MATCHED THEN '
    ||'INSERT (identifiant,code,libelle,fonction,domaine,numero_document,numero_ordre,audit_acteur,audit_fonctionalite,audit_action,audit_date)'
    ||'VALUES (f.fonc_id,f.fonc_code,f.fonc_liblg,case when f.fonc_type like ''G%'' then ''GC'' else ''AGC'' end,''UA''||f.champ_action'
    ||',case when f.fonc_type like ''G%'' then f.sequence_acteur else substr(f.fonc_code,3) end'
    ||',case when f.fonc_type like ''G%'' then f.sequence_acteur else substr(f.fonc_code,3) end'
    ||',:5,:6,:7,:8)'
    ||'WHERE f.fonc_type like ''G%'' OR f.fonc_type like ''A1%'''
    ;
    EXECUTE IMMEDIATE vsql USING audit_acteur,audit_fonctionalite,audit_action_modifier,audit_date,audit_acteur,audit_fonctionalite,audit_action_creer,audit_date;
    COMMIT;
END;

CREATE OR REPLACE PROCEDURE P_IMPORTER_POSTES_ORD(audit_acteur IN VARCHAR2,audit_fonctionalite IN VARCHAR2,audit_action_creer IN VARCHAR2,audit_action_modifier IN VARCHAR2,audit_date IN DATE) AS
    vsql VARCHAR2(4000);
BEGIN
    vsql := 'MERGE INTO SIIBC_ACTEUR.poste p USING SIIBC_MEA.fonction_execution f ON (p.identifiant = f.fonc_id) '
    ||'WHEN MATCHED THEN '
    ||'UPDATE SET p.code = f.fonc_code,p.libelle = f.fonc_liblg,p.audit_acteur = :1,p.audit_fonctionalite = :2,p.audit_action = :3,p.audit_date = :4 '
    ||'WHERE f.fonc_type like ''O%'' OR f.fonc_type like ''A2%'' '
    ||'WHEN NOT MATCHED THEN '
    ||'INSERT (identifiant,code,libelle,fonction,domaine,numero_document,numero_ordre,audit_acteur,audit_fonctionalite,audit_action,audit_date)'
    ||'VALUES (f.fonc_id,f.fonc_code,f.fonc_liblg,case when f.fonc_type like ''O%'' then ''OD'' else ''AOD'' end,''USB''||f.champ_action'
    ||',case when f.fonc_type like ''O%'' then f.sequence_acteur else substr(f.fonc_code,3) end'
    ||',case when f.fonc_type like ''O%'' then f.sequence_acteur else substr(f.fonc_code,3) end'
    ||',:5,:6,:7,:8)'
    ||'WHERE f.fonc_type like ''O%'' OR f.fonc_type like ''A2%'''
    ;
    EXECUTE IMMEDIATE vsql USING audit_acteur,audit_fonctionalite,audit_action_modifier,audit_date,audit_acteur,audit_fonctionalite,audit_action_creer,audit_date;
    COMMIT;
END;

CREATE OR REPLACE PROCEDURE P_IMPORTER_POSTES_CF(audit_acteur IN VARCHAR2,audit_fonctionalite IN VARCHAR2,audit_action_creer IN VARCHAR2,audit_action_modifier IN VARCHAR2,audit_date IN DATE) AS
    vsql VARCHAR2(4000);
BEGIN
    vsql := 'MERGE INTO SIIBC_ACTEUR.poste p USING SIIBC_MEA.fonction_execution f ON (p.identifiant = f.fonc_id) '
    ||'WHEN MATCHED THEN '
    ||'UPDATE SET p.code = f.fonc_code,p.libelle = f.fonc_liblg,p.audit_acteur = :1,p.audit_fonctionalite = :2,p.audit_action = :3,p.audit_date = :4 '
    ||'WHERE f.fonc_type like ''C%'' OR f.fonc_type like ''A3%'' '
    ||'WHEN NOT MATCHED THEN '
    ||'INSERT (identifiant,code,libelle,fonction,domaine,numero_document,numero_ordre,audit_acteur,audit_fonctionalite,audit_action,audit_date)'
    ||'VALUES (f.fonc_id,f.fonc_code,f.fonc_liblg,case when f.fonc_type like ''C%'' then ''CF'' else ''ACF'' end,case when f.loc_id is null then ''SECTION''||f.secb_id else ''LOCALITE''||f.loc_id end '
    ||',case when f.fonc_type like ''C%'' then f.sequence_acteur else substr(f.fonc_code,3) end'
    ||',case when f.fonc_type like ''C%'' then f.sequence_acteur else substr(f.fonc_code,3) end'
    ||',:5,:6,:7,:8)'
    ||'WHERE f.fonc_type like ''C%'' OR f.fonc_type like ''A3%'''
    ;
    EXECUTE IMMEDIATE vsql USING audit_acteur,audit_fonctionalite,audit_action_modifier,audit_date,audit_acteur,audit_fonctionalite,audit_action_creer,audit_date;
    COMMIT;
END;

CREATE OR REPLACE PROCEDURE P_IMPORTER_POSTES_CPT(audit_acteur IN VARCHAR2,audit_fonctionalite IN VARCHAR2,audit_action_creer IN VARCHAR2,audit_action_modifier IN VARCHAR2,audit_date IN DATE) AS
    vsql VARCHAR2(4000);
BEGIN
    vsql := 'MERGE INTO SIIBC_ACTEUR.poste p USING SIIBC_MEA.fonction_execution f ON (p.identifiant = f.fonc_id) '
    ||'WHEN MATCHED THEN '
    ||'UPDATE SET p.code = f.fonc_code,p.libelle = f.fonc_liblg,p.audit_acteur = :1,p.audit_fonctionalite = :2,p.audit_action = :3,p.audit_date = :4 '
    ||'WHERE f.fonc_type like ''T%'' OR f.fonc_type like ''A4%'' '
    ||'WHEN NOT MATCHED THEN '
    ||'INSERT (identifiant,code,libelle,fonction,domaine,numero_document,numero_ordre,audit_acteur,audit_fonctionalite,audit_action,audit_date)'
    ||'VALUES (f.fonc_id,f.fonc_code,f.fonc_liblg,case when f.fonc_type like ''T%'' then ''CA'' else ''ACA'' end,case when f.loc_id is null then ''SECTION''||f.secb_id else ''LOCALITE''||f.loc_id end '
    ||',case when f.fonc_type like ''T%'' then f.sequence_acteur else substr(f.fonc_code,3) end'
    ||',case when f.fonc_type like ''T%'' then f.sequence_acteur else substr(f.fonc_code,3) end'
    ||',:5,:6,:7,:8)'
    ||'WHERE f.fonc_type like ''T%'' OR f.fonc_type like ''A4%'''
    ;
    EXECUTE IMMEDIATE vsql USING audit_acteur,audit_fonctionalite,audit_action_modifier,audit_date,audit_acteur,audit_fonctionalite,audit_action_creer,audit_date;
    COMMIT;
END;

CREATE OR REPLACE PROCEDURE P_IMPORTER_POSTES(audit_acteur IN VARCHAR2,audit_fonctionalite IN VARCHAR2,audit_action_creer IN VARCHAR2,audit_action_modifier IN VARCHAR2,audit_date IN DATE) AS
BEGIN
    P_IMPORTER_POSTES_GC(audit_acteur,audit_fonctionalite,audit_action_creer,audit_action_modifier,audit_date);
    P_IMPORTER_POSTES_ORD(audit_acteur,audit_fonctionalite,audit_action_creer,audit_action_modifier,audit_date);
    P_IMPORTER_POSTES_CF(audit_acteur,audit_fonctionalite,audit_action_creer,audit_action_modifier,audit_date);
    P_IMPORTER_POSTES_CPT(audit_acteur,audit_fonctionalite,audit_action_creer,audit_action_modifier,audit_date);
END;

CREATE OR REPLACE PROCEDURE P_EFFACER_AFFECTATIONS(audit_acteur IN VARCHAR2,audit_fonctionalite IN VARCHAR2,audit_action IN VARCHAR2,audit_date IN DATE) AS
    vsql VARCHAR2(4000);
BEGIN
    vsql := 'UPDATE affectations a SET '||
    -- fonctions budg�taires
    'a.gc = NULL , a.agc = NULL,a.ord = NULL , a.aord = NULL,a.cf = NULL , a.acf = NULL'||',a.cpt = NULL , a.acpt = NULL'||
    -- audit
    ', a.audit_acteur = :1, a.audit_fonctionalite = :2 , a.audit_action = :3 , a.audit_date = :4,a.etat = NULL , a.date_etat = NULL';
    EXECUTE IMMEDIATE vsql USING audit_acteur,audit_fonctionalite,audit_action,audit_date;
    COMMIT;
END;

CREATE OR REPLACE PROCEDURE P_IMPORTER_AFFECTATIONS_NVL(audit_acteur IN VARCHAR2,audit_fonctionalite IN VARCHAR2,audit_action IN VARCHAR2,audit_date IN DATE) AS
BEGIN
    P_RAFFRAICHIR_VM_APP_EX_IMP();
    FOR l IN (SELECT l.* FROM VA_LIGNE_IMPORTABLE l)
    LOOP
        IF l.exercice = 2021 THEN
            INSERT INTO AFFECTATIONS(identifiant,imputation,gc,agc,ord,aord,cf,acf,cpt,acpt,audit_acteur,audit_fonctionalite,audit_action,audit_date) VALUES(l.ldep_id,l.identifiant,l.gc,l.agc,l.od,l.aod,l.cf,l.acf,l.cpt,l.acpt,audit_acteur,audit_fonctionalite,audit_action,audit_date);
        ELSE
            INSERT INTO AFFECTATIONS(identifiant,imputation,gc,agc,ord,aord,cf,acf,cpt,acpt,audit_acteur,audit_fonctionalite,audit_action,audit_date) VALUES(l.identifiant,l.identifiant,l.gc,l.agc,l.od,l.aod,l.cf,l.acf,l.cpt,l.acpt,audit_acteur,audit_fonctionalite,audit_action,audit_date);
        END IF;
    END LOOP;
    COMMIT;
END;

CREATE OR REPLACE PROCEDURE P_IMPORTER_AFFECTATIONS(audit_acteur IN VARCHAR2,audit_fonctionalite IN VARCHAR2,audit_action_creer IN VARCHAR2,audit_action_modifier IN VARCHAR2,audit_date IN DATE) AS
    vsql VARCHAR2(4000);
BEGIN
    vsql := 'MERGE INTO affectations a USING vm_app_ex_imputation i ON (a.identifiant = i.identifiant) '
    ||'WHEN MATCHED THEN UPDATE SET '
    ||'a.gc = i.gc,a.agc = i.agc ,a.ord = i.od,a.aord = i.aod ,a.cf = i.cf,a.acf = i.acf ,a.cpt = i.cpt,a.acpt = i.acpt'
    ||',a.audit_acteur = :1, a.audit_fonctionalite = :2, a.audit_action = :3, a.audit_date = :4 '
    ||',a.etat = i.etat ,a.date_etat = i.date_etat '
    ||'WHEN NOT MATCHED THEN INSERT '
    ||'(identifiant, imputation, gc,agc,ord,aord,cf,acf,cpt,acpt,audit_acteur,audit_fonctionalite,audit_action,audit_date'
    ||',etat,date_etat) values (i.ldep_id,i.identifiant'
    -- fonctions budg�taires
    ||',i.gc,i.agc,i.od,i.aod,i.cf,i.acf,i.cpt,i.acpt'
    -- audit
    ||',:5,:6,:7,:8,NULL,NULL'
    ||')'
    ;
    --DBMS_MVIEW.REFRESH('VM_APP_EX_IMPUTATION', METHOD => 'C', ATOMIC_REFRESH => FALSE,PARALLELISM => 4);
    P_RAFFRAICHIR_VM_APP_EX_IMP();
    EXECUTE IMMEDIATE vsql USING audit_acteur,audit_fonctionalite,audit_action_modifier,audit_date,audit_acteur,audit_fonctionalite,audit_action_creer,audit_date;
    COMMIT;
END;

CREATE OR REPLACE PROCEDURE P_EXPORTER_AFFECTATIONS(audit_acteur IN VARCHAR2,audit_fonctionalite IN VARCHAR2,audit_action IN VARCHAR2,audit_date IN DATE) AS
BEGIN
	FOR l IN (SELECT l.* FROM VA_LIGNE_EXPORTABLE l)
    LOOP
        IF l.exercice = 2021 THEN
            UPDATE ligne_de_depenses@dblink_elabo_bidf u SET
            -- postes
            u.fct_gc_id = l.gc,u.fct_agc_id = l.agc,u.fct_ord_id = l.ord,u.fct_aord_id = l.aord,u.fct_cf_id = l.cf,u.fct_acf_id = l.acf,u.fct_cpt_id = l.cpt,u.fct_acpt_id = l.acpt
    		-- audit
    		,u.etat = l.etat,u.date_etat = l.date_etat
    		WHERE u.ldep_id = l.ldep_id;
        ELSE
            UPDATE ligne_de_depenses@dblink_elabo_bidf u SET
            -- postes
            u.fct_gc_id = l.gc,u.fct_agc_id = l.agc,u.fct_ord_id = l.ord,u.fct_aord_id = l.aord,u.fct_cf_id = l.cf,u.fct_acf_id = l.acf,u.fct_cpt_id = l.cpt,u.fct_acpt_id = l.acpt
    		-- audit
    		,u.etat = l.etat,u.date_etat = l.date_etat
    		WHERE u.exo_num||u.ads_id||u.nat_id = l.identifiant;
        END IF;
    END LOOP;
	
	--MERGE 
    --INTO ligne_de_depenses@dblink_elabo_bidf l 
    --USING ACTEUR.affectations a
    --ON (l.exo_num||l.ads_id||l.nat_id = a.identifiant AND a.etat = 'MODI') 
    --WHEN MATCHED THEN UPDATE SET
    -- fonctions budg�taires
    --l.fct_gc_id = a.gc,l.fct_agc_id = a.agc
    --,l.fct_ord_id = a.ord,l.fct_aord_id = a.aord
    --,l.fct_cf_id = a.cf,l.fct_acf_id = a.acf
    --,l.fct_cpt_id = a.cpt,l.fct_acpt_id = a.acpt
    -- audit
    --,l.etat = a.etat,l.date_etat = a.date_etat;
    COMMIT;
END;

CREATE OR REPLACE PROCEDURE P_IMPORTER_POSTE_AUD_NVL IS
rev INTEGER;
date_ DATE;
timestamp_ INTEGER;
BEGIN
    SELECT TO_DATE('2021/01/01', 'yyyy/mm/dd') INTO date_ FROM DUAL;
    SELECT TRUNC((((TRUNC(date_, 'MI') - DATE '1970-01-01') * 86400 + EXTRACT(SECOND FROM CURRENT_TIMESTAMP)) * 10) / 10) INTO timestamp_ FROM DUAL;
    FOR p IN (
        SELECT p.identifiant,p.code,p.libelle,p.fonction,p.domaine,p.localite,p.audit_acteur,p.audit_fonctionalite,p.audit_action,p.audit_date
        FROM SIIBC_ACTEUR.poste p 
        WHERE p.identifiant NOT IN (SELECT DISTINCT identifiant FROM SIIBC_ACTEUR.poste_aud)
    )LOOP
        SELECT SIIBC_ACTEUR.HIBERNATE_SEQUENCE.NEXTVAL INTO rev FROM DUAL;
        INSERT INTO REVINFO(REV,REVTSTMP) VALUES (rev,timestamp_);        
        INSERT INTO SIIBC_ACTEUR.poste_aud(identifiant, rev, revtype,code,libelle,fonction,domaine,localite
        ,audit_acteur,audit_fonctionalite,audit_action,audit_date) 
        VALUES (p.identifiant,rev,0,p.code,p.libelle,p.fonction,p.domaine,p.localite,NVL(p.audit_acteur,'SYSTEME')
        ,NVL(p.audit_fonctionalite,'INITIALISATION'),NVL(p.audit_action,'CREATION'),NVL(p.audit_date,date_));        
        rev := rev + 1;
    END LOOP;
    COMMIT;
END;

CREATE OR REPLACE PROCEDURE P_IMPORTER_AFF_AUD_NVL IS
rev INTEGER;
date_ DATE;
timestamp_ INTEGER;
BEGIN
    SELECT TO_DATE('2021/01/01', 'yyyy/mm/dd') INTO date_ FROM DUAL;
    SELECT TRUNC((((TRUNC(date_, 'MI') - DATE '1970-01-01') * 86400 + EXTRACT(SECOND FROM CURRENT_TIMESTAMP)) * 10) / 10) INTO timestamp_ FROM DUAL;
    FOR a IN (
        SELECT a.identifiant,a.gc,a.agc,a.ord,a.aord,a.cf,a.acf,a.cpt,a.acpt,a.audit_acteur,a.audit_fonctionalite,a.audit_action,a.audit_date
        FROM SIIBC_ACTEUR.affectations a 
        WHERE a.identifiant NOT IN (SELECT DISTINCT identifiant FROM SIIBC_ACTEUR.affectations_aud)
    )LOOP
        SELECT SIIBC_ACTEUR.HIBERNATE_SEQUENCE.NEXTVAL INTO rev FROM DUAL;
        INSERT INTO REVINFO(REV,REVTSTMP) VALUES (rev,timestamp_);        
        INSERT INTO SIIBC_ACTEUR.affectations_aud(identifiant, rev,revtype,gc,agc,ord,aord,cf,acf,cpt,acpt
        ,audit_acteur,audit_fonctionalite,audit_action,audit_date) 
        VALUES (a.identifiant,rev,0,a.gc,a.agc,a.ord,a.aord,a.cf,a.acf,a.cpt,a.acpt,NVL(a.audit_acteur,'SYSTEME')
        ,NVL(a.audit_fonctionalite,'INITIALISATION'),NVL(a.audit_action,'CREATION'),NVL(a.audit_date,date_));
        rev := rev + 1;
    END LOOP;
    COMMIT;
END;

CREATE OR REPLACE PROCEDURE P_RAFFRAICHIR_VM_APP_PRIVILEGE IS
BEGIN
    DBMS_MVIEW.REFRESH('VM_APP_PRIVILEGE', METHOD => 'C', ATOMIC_REFRESH => FALSE,PARALLELISM => 4);
END;

CREATE OR REPLACE PROCEDURE PA_EXPORTER_POSTE AS
BEGIN
	----insertion des fonctions GC
	MERGE INTO fonction_execution@dblink_elabo_bidf A
	USING (select A.identifiant, A.code, A.libelle,A.numero_document,substr(A.domaine,3) as ua, B.UA_SECB_ID, B.UA_LOC_ID, A.parent, A.fonction
	from poste A
	left outer join SIIBC_CA.unite_administrative B on substr(A.domaine,3)=B.uuid
	where code not in (select fonc_code from fonction_execution@dblink_elabo_bidf)
	and A.fonction='GC') B
	ON (A.fonc_code = B.code)
	WHEN NOT MATCHED THEN INSERT (A.fonc_id, A.FONC_CODE, A.FONC_LIBLG, A.FONC_TYPE, A.SEQUENCE_ACTEUR, A.SECB_ID, A.LOC_ID, A.ETAT, A.DATE_ETAT, A.CHAMP_ACTION, A.PROFIL_CODE)
	VALUES (B.identifiant,B.code,B.libelle, substr(B.code,1,2),B.numero_document,B.ua_secb_id,B.ua_loc_id, 'NOUV',sysdate,B.ua,B.fonction);
	commit;

	----insertion des fonctions AGC
	MERGE INTO fonction_execution@dblink_elabo_bidf A
	USING (select A.identifiant, A.code, A.libelle,A.numero_document,substr(A.domaine,3) as ua, B.UA_SECB_ID, B.UA_LOC_ID, A.parent, A.fonction
	from poste A
	left outer join SIIBC_CA.unite_administrative B on substr(A.domaine,3)=B.uuid
	where code not in (select fonc_code from fonction_execution@dblink_elabo_bidf)
	and A.fonction='AGC') B
	ON (A.fonc_code = B.code)
	WHEN NOT MATCHED THEN INSERT (A.fonc_id, A.FONC_CODE, A.FONC_LIBLG, A.FONC_TYPE, A.SECB_ID, A.LOC_ID, A.ETAT, A.DATE_ETAT, A.CHAMP_ACTION, A.PROFIL_CODE, A.PARENT)
	VALUES (B.identifiant,B.code,B.libelle, substr(B.code,1,2),B.ua_secb_id,B.ua_loc_id, 'NOUV',sysdate,B.ua,B.fonction,B.parent);
	commit;

	----insertion des fonctions ORD
	MERGE INTO fonction_execution@dblink_elabo_bidf A
	USING (select A.identifiant, A.code, A.libelle,A.numero_document,substr(A.domaine,4) as usb, B.USB_SECB_ID, substr(A.localite,9) localite, A.parent, A.fonction
	from poste A
	left outer join SIIBC_CPP.usb B on substr(A.domaine,4)=B.uuid
	where code not in (select fonc_code from fonction_execution@dblink_elabo_bidf)
	and A.fonction='OD') B
	ON (A.fonc_code = B.code)
	WHEN NOT MATCHED THEN INSERT (A.fonc_id, A.FONC_CODE, A.FONC_LIBLG, A.FONC_TYPE, A.SEQUENCE_ACTEUR, A.SECB_ID, A.LOC_ID, A.ETAT, A.DATE_ETAT, A.CHAMP_ACTION, A.PROFIL_CODE)
	VALUES (B.identifiant,B.code,B.libelle, substr(B.code,1,2),B.numero_document,B.usb_secb_id,B.localite, 'NOUV',sysdate,B.usb,B.fonction);
	commit;

	------insertion des fonctions AORD   
	MERGE INTO fonction_execution@dblink_elabo_bidf A
   	USING (select A.identifiant, A.code, A.libelle,A.numero_document,substr(A.domaine,4) as usb, B.USB_SECB_ID, substr(A.localite,9) localite, A.parent, A.fonction
	from poste A
	left outer join SIIBC_CPP.usb B on substr(A.domaine,4)=B.uuid
	where code not in (select fonc_code from fonction_execution@dblink_elabo_bidf)
	and A.fonction='AOD') B
   	ON (A.fonc_code = B.code)
   	WHEN NOT MATCHED THEN INSERT (A.fonc_id, A.FONC_CODE, A.FONC_LIBLG, A.FONC_TYPE, A.SEQUENCE_ACTEUR, A.SECB_ID, A.LOC_ID, A.ETAT, A.DATE_ETAT, A.CHAMP_ACTION, A.PROFIL_CODE, A.PARENT)
   	VALUES (B.identifiant,B.code,B.libelle, substr(B.code,1,2),B.numero_document,B.usb_secb_id,B.localite, 'NOUV',sysdate,B.usb,B.fonction,B.parent);
   	commit;
END;