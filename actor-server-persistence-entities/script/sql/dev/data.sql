CREATE OR REPLACE VIEW VA_IMPUTATION AS
SELECT
	ld.exo_num||ld.ads_id||ld.nat_id AS "IDENTIFIANT" -- To be used to support multiple years
    ,ld.ldep_id AS "LDEP_ID" 
    ,ld.exo_num AS "EXERCICE"
   
    ,ld.exo_num||adp.ads_code||nec.nat_code AS "CODE"
    ,adp.ads_liblg||' | '||nec.nat_liblg AS "LIBELLE"
    ,'CATEGORIE_BUDGET'||cb.uuid AS "CATEGORIE_BUDGET"
    ,'CATEGORIE_BUDGET'||cb.uuid AS "CATEGORIE_BUDGET_IDENTIFIANT"
    ,cb.cbud_code AS "CATEGORIE_BUDGET_CODE"
    ,cb.cbud_code||' '||cb.cbud_liblg AS "CATEGORIE_BUDGET_CODE_LIBELLE"
    ,'SECTION'||s.uuid AS "SECTION"
    ,'SECTION'||s.uuid AS "SECTION_IDENTIFIANT"
    ,s.secb_code AS "SECTION_CODE"
    ,s.secb_code||' '||s.secb_libelle AS "SECTION_CODE_LIBELLE"
    ,'USB'||u.uuid AS "USB"
    ,'USB'||u.uuid AS "USB_IDENTIFIANT"
    ,u.usb_code AS "USB_CODE"
    ,u.usb_code||' '||u.usb_liblg AS "USB_CODE_LIBELLE"
    ,'ACTION'||a.uuid AS "ACTION"
    ,'ACTION'||a.uuid AS "ACTION_IDENTIFIANT"
    ,a.adp_code AS "ACTION_CODE"
    ,a.adp_code||' '||a.adp_liblg AS "ACTION_CODE_LIBELLE"
    ,'ACTIVITE'||adp.ads_id AS "ACTIVITE"
    ,'ACTIVITE'||adp.ads_id AS "ACTIVITE_IDENTIFIANT"
    ,adp.ads_code AS "ACTIVITE_CODE"
    ,adp.ads_code||' '||adp.ads_liblg AS "ACTIVITE_CODE_LIBELLE"
    ,nec.uuid AS "NATURE_ECONOMIQUE"
    ,nec.uuid AS "NATURE_ECONOMIQUE_IDENTIFIANT"
    ,nec.nat_code AS "NATURE_ECONOMIQUE_CODE"
    ,nec.nat_code||' '||nec.nat_liblg AS "NATURE_ECONOMIQUE_CODE_LIBELLE"    
    ,'CATEGORIE_ACTIVITE'||ca.catv_id AS "CATEGORIE_ACTIVITE"
    ,'CATEGORIE_ACTIVITE'||ca.catv_id AS "CA_IDENTIFIANT"
    ,ca.catv_code AS "CA_CODE"
    ,ca.catv_code||' '||ca.catv_liblg AS "CA_CODE_LIBELLE"
    ,nd.uuid AS "NATURE_DEPENSE"
    ,nd.uuid AS "ND_IDENTIFIANT"
    ,nd.ndep_code AS "NATURE_DEPENSE_CODE"
    ,nd.ndep_code AS "ND_CODE" 
    ,nd.ndep_code||' '||nd.ndep_liblg AS "ND_CODE_LIBELLE" 
    ,CASE WHEN s.uuid IS NULL OR ua.uuid IS NULL THEN NULL ELSE 'UA'||ua.uuid END AS "UA"
    ,CASE WHEN s.uuid IS NULL OR ua.uuid IS NULL THEN NULL ELSE 'UA'||ua.uuid END AS "UA_IDENTIFIANT"
    ,CASE WHEN s.uuid IS NULL OR ua.uuid IS NULL THEN NULL ELSE ua.ua_code END AS "UA_CODE"
    ,CASE WHEN s.uuid IS NULL OR ua.uuid IS NULL THEN NULL ELSE ua.ua_code||' '||ua.ua_liblg END AS "UA_CODE_LIBELLE"
    ,CASE WHEN s.uuid IS NULL OR ua.uuid IS NULL THEN NULL ELSE 'UA'||ua.uuid END AS "GESTIONNAIRE"
    ,CASE WHEN s.uuid IS NULL OR ua.uuid IS NULL THEN NULL ELSE gestionnaire.ua_code END AS "GESTIONNAIRE_CODE"
    ,CASE WHEN s.uuid IS NULL OR ua.uuid IS NULL THEN NULL ELSE gestionnaire.ua_code||' '||ua.ua_liblg END AS "GESTIONNAIRE_CODE_LIBELLE"
    ,'LOCALITE'||(CASE WHEN LENGTH(l.loc_code) = 4 THEN (SELECT p.uuid FROM SIIBC_CA.LOCALITE p WHERE p.loc_code = SUBSTR(l.loc_code,1,LENGTH(l.loc_code)-2))
	WHEN LENGTH(l.loc_code) = 6 THEN (SELECT p.uuid FROM SIIBC_CA.LOCALITE p WHERE p.loc_code = SUBSTR(l.loc_code,1,LENGTH(l.loc_code)-4))
	ELSE NULL END) AS "REGION_IDENTIFIANT"	
    ,'LOCALITE'||(CASE WHEN LENGTH(l.loc_code) = 6 THEN (SELECT p.uuid FROM SIIBC_CA.LOCALITE p WHERE p.loc_code = SUBSTR(l.loc_code,1,LENGTH(l.loc_code)-2))
	ELSE NULL END) AS "DEPARTEMENT_IDENTIFIANT"	
    ,'LOCALITE'||l.uuid AS "SOUS_PREFECTURE_IDENTIFIANT"
    ,l.loc_code AS "UA_LOCALITE_CODE"  
    ,localite_gestionnaire.loc_code AS "GESTIONNAIRE_LOCALITE_CODE"
    ,localite_activite.loc_code AS "ACTIVITE_LOCALITE_CODE"
    ,ld.fct_gc_id AS "GC"
    ,ld.fct_agc_id AS "AGC"
    ,ld.fct_ord_id AS "OD"
    ,ld.fct_aord_id AS "AOD"
    ,ld.fct_cf_id AS "CF"
    ,ld.fct_acf_id AS "ACF"
    ,ld.fct_cpt_id AS "CPT"
    ,ld.fct_acpt_id AS "ACPT"
    ,ld.etat
    ,ld.date_etat
FROM
    ligne_de_depenses@dblink_elabo_bidf ld
    ,SIIBC_CPP.categorie_budget cb
    ,SIIBC_ADS.activite_de_service adp
    ,SIIBC_NEC.nature_economique nec
    ,SIIBC_NEC.nature_depense nd
    ,SIIBC_CPP.action a
    ,SIIBC_ADS.categorie_activite ca    
    ,SIIBC_CPP.usb u
    ,SIIBC_CA.section_budgetaire s
    ,SIIBC_CA.unite_administrative ua
    ,SIIBC_CA.unite_administrative gestionnaire
    ,SIIBC_CA.localite l 
    ,SIIBC_CA.localite localite_gestionnaire
    ,SIIBC_CA.localite localite_activite
WHERE
    ld.ads_id = adp.ads_id
    AND ld.cbud_id = cb.uuid (+)
    AND ld.nat_id = nec.uuid
    AND adp.ndep_id = nd.ndep_code
    AND adp.adp_id = a.uuid
    AND adp.catv_id = ca.catv_id
    AND a.adp_usb_id = u.uuid
    AND u.usb_secb_id = s.uuid
    AND ua.uuid (+) = adp.ua_benef_id
    AND gestionnaire.uuid (+) = adp.ua_gest_id
    AND l.uuid (+) = ua.ua_loc_id
    AND localite_gestionnaire.uuid (+) = adp.loc_id
    AND localite_activite.uuid (+) = gestionnaire.ua_loc_id
    --AND ua.ua_secb_id IS NOT NULL
    --AND s.entitystatus = 'COMMITTED'
    
    AND ((ld.fct_gc_id IS NULL) OR (SELECT COUNT(fe.sequence_acteur) FROM fonction_execution@dblink_elabo_bidf fe WHERE fe.fonc_code = ld.fct_gc_id) = 0 OR (SELECT fe.sequence_acteur FROM fonction_execution@dblink_elabo_bidf fe WHERE fe.fonc_code = ld.fct_gc_id) < 90000)
    AND ((ld.fct_ord_id IS NULL) OR (SELECT COUNT(fe.sequence_acteur) FROM fonction_execution@dblink_elabo_bidf fe WHERE fe.fonc_code = ld.fct_ord_id) = 0 OR (SELECT fe.sequence_acteur FROM fonction_execution@dblink_elabo_bidf fe WHERE fe.fonc_code = ld.fct_ord_id) < 90000)
    AND ((ld.fct_cf_id IS NULL) OR (SELECT COUNT(fe.sequence_acteur) FROM fonction_execution@dblink_elabo_bidf fe WHERE fe.fonc_code = ld.fct_cf_id) = 0 OR (SELECT fe.sequence_acteur FROM fonction_execution@dblink_elabo_bidf fe WHERE fe.fonc_code = ld.fct_cf_id) < 90000)
    AND ((ld.fct_cpt_id IS NULL) OR (SELECT COUNT(fe.sequence_acteur) FROM fonction_execution@dblink_elabo_bidf fe WHERE fe.fonc_code = ld.fct_cpt_id) = 0 OR (SELECT fe.sequence_acteur FROM fonction_execution@dblink_elabo_bidf fe WHERE fe.fonc_code = ld.fct_cpt_id) < 90000)
    
    ;
