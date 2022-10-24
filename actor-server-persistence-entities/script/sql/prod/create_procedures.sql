-- Refresh procedure
CREATE OR REPLACE PROCEDURE P_RAFFRAICHIR_VM_APP_SECTION IS
BEGIN
    DBMS_MVIEW.REFRESH('VM_APP_SECTION', METHOD => 'C', ATOMIC_REFRESH => FALSE);
END;

CREATE OR REPLACE PROCEDURE P_RAFFRAICHIR_VM_APP_EX_IMP IS
BEGIN
    DBMS_MVIEW.REFRESH('VM_APP_EX_IMPUTATION', METHOD => 'C', ATOMIC_REFRESH => FALSE,PARALLELISM => 4);
END;

CREATE OR REPLACE PROCEDURE P_EFFACER_AFFECTATIONS(audit_acteur IN VARCHAR2,audit_fonctionalite IN VARCHAR2,audit_action IN VARCHAR2,audit_date IN DATE) AS
    vsql VARCHAR2(4000);
BEGIN
    vsql := 'UPDATE ACTEUR.affectations a SET '||
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

CREATE OR REPLACE PROCEDURE P_IMPORTER_AFFECTATIONS_ACT(audit_acteur IN VARCHAR2,audit_fonctionalite IN VARCHAR2,audit_action_creer IN VARCHAR2,audit_action_modifier IN VARCHAR2,audit_date IN DATE) AS
    vsql VARCHAR2(4000);
BEGIN
    vsql := 'MERGE INTO ACTEUR.affectations a USING ACTEUR.vm_app_ex_imputation i ON (a.identifiant = i.identifiant) '
    ||'WHEN MATCHED THEN UPDATE SET '
    ||'a.gc = i.gc,a.agc = i.agc ,a.ord = i.od,a.aord = i.aod ,a.cf = i.cf,a.acf = i.acf ,a.cpt = i.cpt,a.acpt = i.acpt'
    ||',a.audit_acteur = :1, a.audit_fonctionalite = :2, a.audit_action = :3, a.audit_date = :4 '
    ||',a.etat = i.etat ,a.date_etat = i.date_etat '
    ;
    P_RAFFRAICHIR_VM_APP_EX_IMP();
    EXECUTE IMMEDIATE vsql USING audit_acteur,audit_fonctionalite,audit_action_modifier,audit_date;
    COMMIT;
END;

CREATE OR REPLACE PROCEDURE P_IMPORTER_AFFECTATIONS(audit_acteur IN VARCHAR2,audit_fonctionalite IN VARCHAR2,audit_action_creer IN VARCHAR2,audit_action_modifier IN VARCHAR2,audit_date IN DATE) AS
    vsql VARCHAR2(4000);
BEGIN
    vsql := 'MERGE INTO ACTEUR.affectations a USING ACTEUR.vm_app_ex_imputation i ON (a.identifiant = i.identifiant) '
    ||'WHEN MATCHED THEN UPDATE SET '
    ||'a.gc = i.gc,a.agc = i.agc ,a.ord = i.od,a.aord = i.aod ,a.cf = i.cf,a.acf = i.acf ,a.cpt = i.cpt,a.acpt = i.acpt'
    ||',a.audit_acteur = :1, a.audit_fonctionalite = :2, a.audit_action = :3, a.audit_date = :4 '
    ||',a.etat = i.etat ,a.date_etat = i.date_etat '
    ||'WHEN NOT MATCHED THEN INSERT '
    ||'(identifiant, imputation, gc,agc,ord,aord,cf,acf,cpt,acpt,audit_acteur,audit_fonctionalite,audit_action,audit_date'
    ||',etat,date_etat) values (i.identifiant,i.identifiant'
    -- fonctions budg�taires
    ||',i.gc,i.agc,i.od,i.aod,i.cf,i.acf,i.cpt,i.acpt'
    -- audit
    ||',:5,:6,:7,:8,NULL,NULL'
    ||')'
    ;
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
    		,u.etat = l.etat_bidf,u.date_etat = l.date_etat
    		WHERE u.ldep_id = l.ldep_id AND u.exo_num = l.exercice;
        ELSE
            UPDATE ligne_de_depenses@dblink_elabo_bidf u SET
            -- postes
            u.fct_gc_id = l.gc,u.fct_agc_id = l.agc,u.fct_ord_id = l.ord,u.fct_aord_id = l.aord,u.fct_cf_id = l.cf,u.fct_acf_id = l.acf,u.fct_cpt_id = l.cpt,u.fct_acpt_id = l.acpt
    		-- audit
    		,u.etat = l.etat_bidf,u.date_etat = l.date_etat
    		WHERE u.ldep_id = l.ldep_id AND u.exo_num = l.exercice;
        END IF;
    END LOOP;
    
    --
    UPDATE affectations
    SET etat='PEC', date_etat=sysdate
    WHERE etat='MODI';
	
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
	left outer join CA.unite_administrative B on substr(A.domaine,3)=B.uuid
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
	left outer join CA.unite_administrative B on substr(A.domaine,3)=B.uuid
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
	left outer join CPP.usb B on substr(A.domaine,4)=B.uuid
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
	left outer join CPP.usb B on substr(A.domaine,4)=B.uuid
	where code not in (select fonc_code from fonction_execution@dblink_elabo_bidf)
	and A.fonction='AOD') B
   	ON (A.fonc_code = B.code)
   	WHEN NOT MATCHED THEN INSERT (A.fonc_id, A.FONC_CODE, A.FONC_LIBLG, A.FONC_TYPE, A.SECB_ID, A.LOC_ID, A.ETAT, A.DATE_ETAT, A.CHAMP_ACTION, A.PROFIL_CODE, A.PARENT)
   	VALUES (B.identifiant,B.code,B.libelle, substr(B.code,1,2),B.usb_secb_id,B.localite, 'NOUV',sysdate,B.usb,B.fonction,B.parent);
   	commit;
END;