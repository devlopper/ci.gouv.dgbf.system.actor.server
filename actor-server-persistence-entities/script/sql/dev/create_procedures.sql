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

CREATE OR REPLACE PROCEDURE P_IMPORTER_AFFECTATIONS(audit_acteur IN VARCHAR2,audit_fonctionalite IN VARCHAR2,audit_action_creer IN VARCHAR2,audit_action_modifier IN VARCHAR2,audit_date IN DATE) AS
    vsql VARCHAR2(4000);
BEGIN
    vsql := 'MERGE INTO affectations a USING vm_app_ex_imputation i ON (a.identifiant = i.ldep_id) '
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
MERGE 
    INTO SIIBC_MEA.ligne_de_depenses l 
    USING SIIBC_ACTEUR.affectations a 
    ON (l.ldep_id = a.identifiant AND a.etat = 'MODI') 
    WHEN MATCHED THEN UPDATE SET
    -- fonctions budg�taires
    l.fct_gc_id = a.gc,l.fct_agc_id = a.agc
    ,l.fct_ord_id = a.ord,l.fct_aord_id = a.aord
    ,l.fct_cf_id = a.cf,l.fct_acf_id = a.acf
    ,l.fct_cpt_id = a.cpt,l.fct_acpt_id = a.acpt
    -- audit
    ,l.etat = a.etat,l.date_etat = a.date_etat;
    COMMIT;
END;