-----------------------------------------------------------------------------------------------------------------------------------------------------------------------
-- Classification administrative - CA                                                                                                                                --
-----------------------------------------------------------------------------------------------------------------------------------------------------------------------

-- Section
DROP MATERIALIZED VIEW "ACTEUR".VM_APP_SECTION;
CREATE MATERIALIZED VIEW "ACTEUR".VM_APP_SECTION
REFRESH ON COMMIT COMPLETE AS
SELECT 'SECTION'||s.uuid AS "IDENTIFIANT",s.secb_code AS "CODE",s.secb_libelle AS "LIBELLE"
FROM CA.section_budgetaire s,CA.gouvernement g
WHERE s.gouv_id = g.uuid AND g.gouv_statut = 'ENABLED' AND g.gouv_utilise = 1 AND s.entitystatus = 'COMMITTED';
ALTER TABLE VM_APP_SECTION ADD CONSTRAINT VM_APP_SECTION_PK PRIMARY KEY (IDENTIFIANT);
ALTER TABLE VM_APP_SECTION ADD CONSTRAINT VM_APP_SECTION_UK_CODE UNIQUE (CODE);

-- Unité administrative
DROP MATERIALIZED VIEW "ACTEUR".VM_APP_UNITE_ADMINISTRATIVE;
CREATE MATERIALIZED VIEW "ACTEUR".VM_APP_UNITE_ADMINISTRATIVE
REFRESH ON COMMIT COMPLETE AS
SELECT 'UA'||ua.uuid AS "IDENTIFIANT",ua.ua_code AS "CODE",ua.ua_liblg AS "LIBELLE"
-- Section
    ,CASE WHEN s.uuid IS NULL OR s.entitystatus <> 'COMMITTED' THEN NULL ELSE 'SECTION'||s.uuid END AS "SECTION"
    ,CASE WHEN s.uuid IS NULL OR s.entitystatus <> 'COMMITTED' THEN NULL ELSE 'SECTION'||s.uuid END AS "SECTION_IDENTIFIANT"
    ,CASE WHEN s.uuid IS NULL OR s.entitystatus <> 'COMMITTED' THEN NULL ELSE s.secb_code||' '||s.secb_libelle END AS "SECTION_CODE_LIBELLE"
-- Localité
    ,CASE WHEN l.uuid IS NULL OR l.entitystatus <> 'COMMITTED' THEN NULL ELSE 'LOCALITE'||l.uuid END AS "LOCALITE"
FROM CA.unite_administrative ua,CA.section_budgetaire s,CA.localite l
WHERE 
    s.uuid (+) = ua.ua_secb_id AND l.uuid (+) = ua.ua_loc_id
    --AND s.entitystatus = 'COMMITTED'
    --AND g.uuid = s.gouv_id 
    --AND g.gouv_statut = 'ENABLED' 
    --AND g.gouv_utilise = 1
    ;
ALTER TABLE VM_APP_UNITE_ADMINISTRATIVE ADD CONSTRAINT VM_APP_UA_PK PRIMARY KEY (IDENTIFIANT);
ALTER TABLE VM_APP_UNITE_ADMINISTRATIVE ADD CONSTRAINT VM_APP_UA_UK_CODE UNIQUE (CODE);
CREATE INDEX VM_APP_UA_K_SECTION ON VM_APP_UNITE_ADMINISTRATIVE (SECTION ASC);

-- Localité
DROP MATERIALIZED VIEW "ACTEUR".VM_APP_LOCALITE;
CREATE MATERIALIZED VIEW "ACTEUR".VM_APP_LOCALITE
REFRESH NEXT SYSDATE + 1/48 COMPLETE AS
SELECT 'LOCALITE'||l.uuid AS "IDENTIFIANT",l.loc_code AS "CODE",l.loc_lib AS "LIBELLE"
,CASE WHEN LENGTH(l.loc_code) = 2 AND (LOWER(l.loc_lib) LIKE 'region%' OR LOWER(l.loc_lib) LIKE 'région%') THEN 'REGION' 
WHEN LENGTH(l.loc_code) = 4 THEN 'DEPARTEMENT' ELSE 'SOUS_PREFECTURE' END AS "TYPE"
,(CASE WHEN LENGTH(l.loc_code) = 2 AND (LOWER(l.loc_lib) LIKE 'region%' OR LOWER(l.loc_lib) LIKE 'région%') THEN NULL 
WHEN LENGTH(l.loc_code) >=4  THEN (SELECT 'LOCALITE'||p.uuid FROM CA.LOCALITE p WHERE p.loc_code = SUBSTR(l.loc_code,1,LENGTH(l.loc_code)-2))
--WHEN LENGTH(l.loc_code) = 4 THEN (SELECT 'LOCALITE'||p.uuid FROM SIIBC_CA.LOCALITE p WHERE p.loc_code = SUBSTR(l.loc_code,1,2))
--WHEN LENGTH(l.loc_code) = 6 THEN (SELECT 'LOCALITE'||p.uuid FROM SIIBC_CA.LOCALITE p WHERE p.loc_code = SUBSTR(l.loc_code,1,4))
ELSE NULL END) AS "PARENT"
-- Département
,(CASE WHEN LENGTH(l.loc_code) = 6 THEN 'LOCALITE'||(SELECT p.uuid FROM CA.LOCALITE p WHERE p.loc_code = SUBSTR(l.loc_code,1,LENGTH(l.loc_code)-2))
ELSE NULL END) AS "DEPARTEMENT_IDENTIFIANT"
,(CASE WHEN LENGTH(l.loc_code) = 6 THEN (SELECT p.loc_code||' '||p.loc_lib FROM CA.LOCALITE p WHERE p.loc_code = SUBSTR(l.loc_code,1,LENGTH(l.loc_code)-2))
ELSE NULL END) AS "DEPARTEMENT_CODE_LIBELLE"
-- Région
,(CASE WHEN LENGTH(l.loc_code) = 4 THEN 'LOCALITE'||(SELECT p.uuid FROM CA.LOCALITE p WHERE p.loc_code = SUBSTR(l.loc_code,1,LENGTH(l.loc_code)-2))
WHEN LENGTH(l.loc_code) = 6 THEN 'LOCALITE'||(SELECT p.uuid FROM CA.LOCALITE p WHERE p.loc_code = SUBSTR(l.loc_code,1,LENGTH(l.loc_code)-4))
ELSE NULL END) AS "REGION_IDENTIFIANT"
,(CASE WHEN LENGTH(l.loc_code) = 4 THEN (SELECT p.loc_code||' '||p.loc_lib FROM CA.LOCALITE p WHERE p.loc_code = SUBSTR(l.loc_code,1,LENGTH(l.loc_code)-2))
WHEN LENGTH(l.loc_code) = 6 THEN (SELECT p.loc_code||' '||p.loc_lib FROM CA.LOCALITE p WHERE p.loc_code = SUBSTR(l.loc_code,1,LENGTH(l.loc_code)-4))
ELSE NULL END) AS "REGION_CODE_LIBELLE"
,l.loc_tref_id AS "REFERENTIEL"
FROM CA.localite l WHERE l.entitystatus = 'COMMITTED';
ALTER TABLE VM_APP_LOCALITE ADD CONSTRAINT VM_APP_LOCALITE_PK PRIMARY KEY (IDENTIFIANT);
ALTER TABLE VM_APP_LOCALITE ADD CONSTRAINT VM_APP_LOCALITE_UK_CODE UNIQUE (CODE,REFERENTIEL);

-----------------------------------------------------------------------------------------------------------------------------------------------------------------------
-- Classification par programme - CPP                                                                                                                                --
-----------------------------------------------------------------------------------------------------------------------------------------------------------------------

-- Catégorie de budget
DROP MATERIALIZED VIEW "ACTEUR".VM_APP_CAT_BUDGET;
CREATE MATERIALIZED VIEW "ACTEUR".VM_APP_CAT_BUDGET
REFRESH ON COMMIT COMPLETE AS
SELECT 'CATEGORIE_BUDGET'||cb.uuid AS "IDENTIFIANT",cb.cbud_code AS "CODE",cb.cbud_liblg AS "LIBELLE"
FROM CPP.CATEGORIE_BUDGET cb
WHERE cb.entitystatus = 'COMMITTED';
ALTER TABLE VM_APP_CAT_BUDGET ADD CONSTRAINT VM_APP_CAT_BUDGET_PK PRIMARY KEY (IDENTIFIANT);
ALTER TABLE VM_APP_CAT_BUDGET ADD CONSTRAINT VM_APP_CAT_BUDGET_UK_CODE UNIQUE (CODE);

-- Unité de spécialisation du budget
DROP MATERIALIZED VIEW "ACTEUR".VM_APP_USB;
CREATE MATERIALIZED VIEW "ACTEUR".VM_APP_USB
REFRESH ON COMMIT COMPLETE AS
SELECT 'USB'||usb.uuid AS "IDENTIFIANT",usb.usb_code AS "CODE",usb.usb_liblg AS "LIBELLE"
    ,'SECTION'||s.uuid AS "SECTION",'SECTION'||s.uuid AS "SECTION_IDENTIFIANT",s.secb_code||' '||s.secb_libelle AS "SECTION_CODE_LIBELLE"
    ,'CATEGORIE_BUDGET'||usb.usb_cbud_id AS "CATEGORIE"
    ,'CATEGORIE_BUDGET'||usb.usb_cbud_id AS "CATEGORIE_IDENTIFIANT"
    ,cb.cbud_code||' '||cb.cbud_liblg AS "CATEGORIE_CODE_LIBELLE"
FROM CPP.usb usb,CPP.categorie_budget cb,CA.section_budgetaire s
WHERE s.uuid = usb.usb_secb_id AND s.entitystatus = 'COMMITTED' AND cb.entitystatus = 'COMMITTED' AND usb.usb_cbud_id = cb.uuid AND cb.entitystatus = 'COMMITTED';
ALTER TABLE VM_APP_USB ADD CONSTRAINT VM_APP_USB_PK PRIMARY KEY (IDENTIFIANT);
ALTER TABLE VM_APP_USB ADD CONSTRAINT VM_APP_USB_UK_CODE UNIQUE (CODE);
CREATE INDEX VM_APP_USB_K_CATEGORIE ON VM_APP_USB (CATEGORIE ASC);
CREATE INDEX VM_APP_USB_K_SECTION ON VM_APP_USB (SECTION ASC);

-- Action
DROP MATERIALIZED VIEW "ACTEUR".VM_APP_ACTION;
CREATE MATERIALIZED VIEW "ACTEUR".VM_APP_ACTION
REFRESH ON COMMIT COMPLETE AS
SELECT 'ACTION'||a.uuid AS "IDENTIFIANT",a.adp_code AS "CODE",a.adp_liblg AS "LIBELLE"
    ,'SECTION'||s.uuid AS "SECTION",'SECTION'||s.uuid AS "SECTION_IDENTIFIANT",s.secb_code||' '||s.secb_libelle AS "SECTION_CODE_LIBELLE"
    ,'USB'||usb.uuid AS "USB",'USB'||usb.uuid AS "USB_IDENTIFIANT",usb.usb_code||' '||usb.usb_liblg AS "USB_CODE_LIBELLE"
    ,'CATEGORIE_BUDGET'||usb.usb_cbud_id AS "CATEGORIE_BUDGET"
    ,'CATEGORIE_BUDGET'||usb.usb_cbud_id AS "CATEGORIE_BUDGET_IDENTIFIANT"
    ,cb.cbud_code||' '||cb.cbud_liblg AS "CATEGORIE_BUDGET_CODE_LIBELLE"
FROM CPP.action a,CPP.usb usb,CPP.categorie_budget cb,CA.section_budgetaire s
WHERE a.adp_usb_id = usb.uuid AND s.uuid = usb.usb_secb_id AND cb.entitystatus = 'COMMITTED' AND usb.usb_cbud_id = cb.uuid AND cb.entitystatus = 'COMMITTED';
ALTER TABLE VM_APP_ACTION ADD CONSTRAINT VM_APP_ACTION_PK PRIMARY KEY (IDENTIFIANT);
ALTER TABLE VM_APP_ACTION ADD CONSTRAINT VM_APP_ACTION_UK_CODE UNIQUE (CODE);
CREATE INDEX VM_APP_ACTION_K_CB ON VM_APP_ACTION (CATEGORIE_BUDGET ASC);
CREATE INDEX VM_APP_ACTION_K_SECTION ON VM_APP_ACTION (SECTION ASC);
CREATE INDEX VM_APP_ACTION_K_USB ON VM_APP_ACTION (USB ASC);

-----------------------------------------------------------------------------------------------------------------------------------------------------------------------
-- Activité de service - ADS                                                                                                                                         --
-----------------------------------------------------------------------------------------------------------------------------------------------------------------------

-- Catégorie Activité
DROP MATERIALIZED VIEW "ACTEUR".VM_APP_CATEG_ATV;
CREATE MATERIALIZED VIEW "ACTEUR".VM_APP_CATEG_ATV
REFRESH ON COMMIT COMPLETE AS
SELECT 'CATEGORIE_ACTIVITE'||ca.catv_id AS "IDENTIFIANT",ca.catv_code AS "CODE",ca.catv_liblg AS "LIBELLE"
FROM ADS.categorie_activite ca;
ALTER TABLE VM_APP_CATEG_ATV ADD CONSTRAINT VM_APP_CATEG_ATV_UK_ID UNIQUE (IDENTIFIANT);
--ALTER TABLE VM_APP_CATEG_ATV ADD CONSTRAINT VM_APP_CATEG_ATV_PK PRIMARY KEY (IDENTIFIANT);
--ALTER TABLE VM_APP_CATEG_ATV ADD CONSTRAINT VM_APP_CATEG_ATV_UK_CODE UNIQUE (CODE);

-- Activité
DROP MATERIALIZED VIEW "ACTEUR".VM_APP_ACTIVITE;
CREATE MATERIALIZED VIEW "ACTEUR".VM_APP_ACTIVITE
REFRESH ON COMMIT COMPLETE AS
SELECT 'ACTIVITE'||a.ads_id AS "IDENTIFIANT",a.ads_code AS "CODE",a.ads_liblg AS "LIBELLE"
    ,'CATEGORIE_ACTIVITE'||c.catv_id AS "CATEGORIE",c.catv_code||' '||c.catv_liblg AS "CATEGORIE_CODE_LIBELLE"
    ,CASE WHEN action.uuid IS NULL THEN NULL ELSE 'ACTION'||action.uuid END AS "ACTION",action.adp_code||' '||action.adp_liblg AS "ACTION_CODE_LIBELLE"
    ,CASE WHEN usb.uuid IS NULL THEN NULL ELSE 'USB'||usb.uuid END AS "USB",usb.usb_code||' '||usb.usb_liblg AS "USB_CODE_LIBELLE"
    ,CASE WHEN section.uuid IS NULL THEN NULL ELSE 'SECTION'||section.uuid END AS "SECTION",section.secb_code||' '||section.secb_libelle AS "SECTION_CODE_LIBELLE"
    ,CASE WHEN nd.uuid IS NULL THEN NULL ELSE nd.uuid END AS "NATURE_DEPENSE",nd.ndep_code||' '||nd.ndep_libct AS "NATURE_DEPENSE_CODE_LIBELLE"
    ,CASE WHEN ua.uuid IS NULL THEN NULL ELSE 'UA'||ua.uuid END AS "UA"
    ,CASE WHEN ua.ua_code IS NULL THEN NULL ELSE ua.ua_code||' '||ua.ua_liblg END AS "UA_CODE_LIBELLE"
FROM ADS.activite_de_service a,ADS.categorie_activite c,CPP.action action,CPP.usb usb,CA.section_budgetaire section
,NEC.nature_depense nd,CA.unite_administrative ua
WHERE a.catv_id = c.catv_id (+) AND a.adp_id = action.uuid (+) AND action.adp_usb_id = usb.uuid (+) 
AND usb.usb_secb_id = section.uuid (+) AND nd.ndep_code = a.ndep_id (+) AND ua.uuid (+) = a.ua_benef_id;
ALTER TABLE VM_APP_ACTIVITE ADD CONSTRAINT VM_APP_ACTIVITE_PK PRIMARY KEY (IDENTIFIANT);
ALTER TABLE VM_APP_ACTIVITE ADD CONSTRAINT VM_APP_ACTIVITE_UK_CODE UNIQUE (CODE);
CREATE INDEX VM_APP_ACTIVITE_K_SECTION ON VM_APP_ACTIVITE (SECTION ASC);
CREATE INDEX VM_APP_ACTIVITE_K_USB ON VM_APP_ACTIVITE (USB ASC);
CREATE INDEX VM_APP_ACTIVITE_K_ACTION ON VM_APP_ACTIVITE (ACTION ASC);
CREATE INDEX VM_APP_ACTIVITE_K_ND ON VM_APP_ACTIVITE (NATURE_DEPENSE ASC);
CREATE INDEX VM_APP_ACTIVITE_K_UA ON VM_APP_ACTIVITE (UA ASC);

-----------------------------------------------------------------------------------------------------------------------------------------------------------------------
-- Budgétisation - BUDGET                                                                                                                                         --
-----------------------------------------------------------------------------------------------------------------------------------------------------------------------

-- Imputation
DROP MATERIALIZED VIEW "ACTEUR".VM_APP_IMPUTATION;
CREATE MATERIALIZED VIEW "ACTEUR".VM_APP_IMPUTATION
TABLESPACE USERS
-- REFRESH ON COMMIT
REFRESH NEXT SYSDATE + 1/24 COMPLETE AS
SELECT
    ld.uuid AS "LDEP_ID"
    ,ld.dep_activite_id||ld.dep_nature_eco_id AS "IDENTIFIANT",adp.ads_code||nec.nat_code AS "CODE",adp.ads_liblg||' | '||nec.nat_liblg AS "LIBELLE"
    ,'ACTIVITE'||adp.ads_id AS "ACTIVITE",'ACTIVITE'||adp.ads_id AS "ACTIVITE_IDENTIFIANT",adp.ads_code AS "ACTIVITE_CODE",adp.ads_code||' '||adp.ads_liblg AS "ACTIVITE_CODE_LIBELLE"
    ,nec.uuid AS "NATURE_ECONOMIQUE",nec.uuid AS "NATURE_ECONOMIQUE_IDENTIFIANT",nec.nat_code AS "NATURE_ECONOMIQUE_CODE",nec.nat_code||' '||nec.nat_liblg AS "NATURE_ECONOMIQUE_CODE_LIBELLE"
    ,'ACTION'||a.uuid AS "ACTION",'ACTION'||a.uuid AS "ACTION_IDENTIFIANT",a.adp_code AS "ACTION_CODE",a.adp_code||' '||a.adp_liblg AS "ACTION_CODE_LIBELLE"
    ,'USB'||u.uuid AS "USB",'USB'||u.uuid AS "USB_IDENTIFIANT",u.usb_code AS "USB_CODE",u.usb_code||' '||u.usb_liblg AS "USB_CODE_LIBELLE"
    ,'SECTION'||s.uuid AS "SECTION",'SECTION'||s.uuid AS "SECTION_IDENTIFIANT",s.secb_code AS "SECTION_CODE",s.secb_code||' '||s.secb_libelle AS "SECTION_CODE_LIBELLE"
    ,'UA'||ua.uuid AS "UA",'UA'||ua.uuid AS "UA_IDENTIFIANT",ua.ua_code AS "UA_CODE",ua.ua_code||' '||ua.ua_liblg AS "UA_CODE_LIBELLE"
    ,'UA'||ua.uuid AS "GESTIONNAIRE",gestionnaire.ua_code AS "GESTIONNAIRE_CODE",gestionnaire.ua_code||' '||ua.ua_liblg AS "GESTIONNAIRE_CODE_LIBELLE"
    ,l.loc_code AS "UA_LOCALITE_CODE"
    ,'CATEGORIE_ACTIVITE'||ca.catv_id AS "CATEGORIE_ACTIVITE",'CATEGORIE_ACTIVITE'||ca.catv_id AS "CA_IDENTIFIANT",ca.catv_code AS "CA_CODE",ca.catv_code||' '||ca.catv_liblg AS "CA_CODE_LIBELLE"
    ,nd.uuid AS "ND_IDENTIFIANT" ,nd.ndep_code AS "ND_CODE" ,nd.ndep_code||' '||nd.ndep_liblg AS "ND_CODE_LIBELLE" 
    
    ,'LOCALITE'||(CASE WHEN LENGTH(l.loc_code) = 4 THEN (SELECT p.uuid FROM CA.LOCALITE p WHERE p.loc_code = SUBSTR(l.loc_code,1,LENGTH(l.loc_code)-2))
	WHEN LENGTH(l.loc_code) = 6 THEN (SELECT p.uuid FROM CA.LOCALITE p WHERE p.loc_code = SUBSTR(l.loc_code,1,LENGTH(l.loc_code)-4))	
	ELSE NULL END) AS "REGION_IDENTIFIANT"
	
    ,'LOCALITE'||(CASE WHEN LENGTH(l.loc_code) = 6 THEN (SELECT p.uuid FROM CA.LOCALITE p WHERE p.loc_code = SUBSTR(l.loc_code,1,LENGTH(l.loc_code)-2))
	ELSE NULL END) AS "DEPARTEMENT_IDENTIFIANT"
	
    ,'LOCALITE'||l.uuid AS "SOUS_PREFECTURE_IDENTIFIANT"
    ,localite_gestionnaire.loc_code AS "GESTIONNAIRE_LOCALITE_CODE"
    ,localite_activite.loc_code AS "ACTIVITE_LOCALITE_CODE"
FROM
    BUDGET.ligne_depense ld
    ,ADS.activite_de_service adp
    ,NEC.nature_economique nec
    ,NEC.nature_depense nd
    ,CPP.action a
    ,ADS.categorie_activite ca    
    ,CPP.usb u
    ,CA.section_budgetaire s
    ,CA.unite_administrative ua
    ,CA.localite l
    ,CA.unite_administrative gestionnaire
    ,CA.localite localite_gestionnaire
    ,CA.localite localite_activite
WHERE
    ld.dep_activite_id = adp.ads_id
    AND ld.dep_nature_eco_id = nec.uuid
    AND adp.ndep_id = nd.ndep_code
    AND adp.adp_id = a.uuid
    AND adp.catv_id = ca.catv_id
    AND u.usb_secb_id = s.uuid
    AND a.adp_usb_id = u.uuid
    AND ua.uuid = adp.ua_benef_id
    AND gestionnaire.uuid = adp.ua_gest_id
    AND l.uuid (+) = ua.ua_loc_id
    AND localite_gestionnaire.uuid (+) = adp.loc_id
    AND localite_activite.uuid (+) = gestionnaire.ua_loc_id
    AND ua.ua_secb_id IS NOT NULL
    AND s.entitystatus = 'COMMITTED'
    ;
ALTER TABLE VM_APP_IMPUTATION ADD CONSTRAINT VM_APP_IMPUTATION_PK PRIMARY KEY (IDENTIFIANT);
CREATE INDEX VM_APP_IMPUTATION_K_SECTION ON VM_APP_IMPUTATION (SECTION ASC);
CREATE INDEX VM_APP_IMPUTATION_K_USB ON VM_APP_IMPUTATION (USB ASC);
CREATE INDEX VM_APP_IMPUTATION_K_ACTION ON VM_APP_IMPUTATION (ACTION ASC);
CREATE INDEX VM_APP_IMPUTATION_K_ACTIVITE ON VM_APP_IMPUTATION (ACTIVITE ASC);
CREATE INDEX VM_APP_IMPUTATION_K_NEC ON VM_APP_IMPUTATION (NATURE_ECONOMIQUE ASC);
CREATE INDEX VM_APP_IMPUTATION_K_UA ON VM_APP_IMPUTATION (UA ASC);

-----------------------------------------------------------------------------------------------------------------------------------------------------------------------
-- Portail - PRT                                                                                                                                --
-----------------------------------------------------------------------------------------------------------------------------------------------------------------------

-- Module
DROP MATERIALIZED VIEW "ACTEUR".VM_APP_MODULE;
CREATE MATERIALIZED VIEW "ACTEUR".VM_APP_MODULE
REFRESH ON COMMIT COMPLETE AS
SELECT 'MODULE'||t.id AS "IDENTIFIANT",t.id AS "CODE",t.name AS "LIBELLE"
FROM PRT.msvc_module t;

-- Service
DROP MATERIALIZED VIEW "ACTEUR".VM_APP_SERVICE;
CREATE MATERIALIZED VIEW "ACTEUR".VM_APP_SERVICE
REFRESH ON COMMIT COMPLETE AS
SELECT 'SERVICE'||t.id AS "IDENTIFIANT",CASE WHEN t.url = '/acteur/private/index.jsf' THEN 'mic-acteur' ELSE t.url END AS "CODE",t.name AS "LIBELLE"
    ,'MODULE'||t.id_module AS "MODULE",m.code||' '||m.name AS "MODULE_CODE_LIBELLE",COUNT(me.id) AS "NOMBRE_DE_MENUS"
FROM PRT.msvc_service t,PRT.msvc_module m,PRT.msvc_menu me
WHERE t.id_module = m.id AND t.id = me.id_service
GROUP BY 'SERVICE'||t.id,CASE WHEN t.url = '/acteur/private/index.jsf' THEN 'mic-acteur' ELSE t.url END,t.name,'MODULE'||t.id_module,m.code||' '||m.name;
ALTER TABLE VM_APP_SERVICE ADD CONSTRAINT VM_APP_SERVICE_PK PRIMARY KEY (IDENTIFIANT);
-- Menu
DROP MATERIALIZED VIEW "ACTEUR".VM_APP_MENU;
CREATE MATERIALIZED VIEW "ACTEUR".VM_APP_MENU
REFRESH ON COMMIT COMPLETE AS
SELECT 'MENU'||t.id AS "IDENTIFIANT",t.code AS "CODE",t.name AS "LIBELLE",t.url AS "URL"
    ,'SERVICE'||t.id_service AS "SERVICE",s.code||' '||s.name AS "SERVICE_CODE_LIBELLE",m.code||' '||m.name AS "MODULE_CODE_LIBELLE"
    --,CASE WHEN t.menuparent_id IS NULL THEN 'SERVICE'||t.id_service ELSE 'MENU'||t.menuparent_id END AS "PARENT"
FROM PRT.msvc_menu t,PRT.msvc_service s,PRT.msvc_module m
WHERE t.id_service = s.id AND s.id_module = m.id AND t.abstrait = 0;
ALTER TABLE VM_APP_MENU ADD CONSTRAINT VM_APP_MENU_PK PRIMARY KEY (IDENTIFIANT);
ALTER TABLE VM_APP_MENU ADD CONSTRAINT VM_APP_MENU_UK_CODE UNIQUE (CODE);
-- Privilége
DROP MATERIALIZED VIEW "ACTEUR".VM_APP_PRIVILEGE;
CREATE MATERIALIZED VIEW "ACTEUR".VM_APP_PRIVILEGE
--REFRESH ON COMMIT 
REFRESH NEXT SYSDATE + 1/24
COMPLETE
AS
SELECT 
    'MODULE'||t.id AS "IDENTIFIANT"
    ,t.id AS "CODE"
    ,t.name AS "LIBELLE"
    ,NULL AS "PARENT"
    ,'MODULE' AS "TYPE"
FROM 
    PRT.msvc_module t
UNION ALL
SELECT 
    'SERVICE'||t.id AS "IDENTIFIANT"
    ,t.code AS "CODE"
    ,t.name AS "LIBELLE"
    ,'MODULE'||t.id_module AS "PARENT"
    ,'SERVICE' AS "TYPE"
FROM  
    PRT.msvc_service t
UNION ALL
SELECT
    'MENU'||t.id AS "IDENTIFIANT"
    ,t.code AS "CODE"
    ,t.name AS "LIBELLE"
    --,'SERVICE'||t.id_service AS "PARENT"
    ,CASE WHEN t.menuparent_id IS NULL THEN 'SERVICE'||t.id_service ELSE 'MENU'||t.menuparent_id END AS "PARENT"
    ,'MENU' AS "TYPE"
FROM
    PRT.msvc_menu t
UNION ALL
SELECT
    'ACTION'||t.id AS "IDENTIFIANT"
    ,t.code AS "CODE"
    ,t.libelle AS "LIBELLE"
    ,'MENU'||t.id_menu AS "PARENT"
    ,'ACTION' AS "TYPE"
FROM
    PRT.msvc_action t
;
ALTER TABLE VM_APP_PRIVILEGE ADD CONSTRAINT VM_APP_PRVLG_PK PRIMARY KEY (IDENTIFIANT);
--ALTER TABLE VM_APP_PRIVILEGE ADD CONSTRAINT VM_APP_PRVLG_UK_CODE UNIQUE (CODE);
CREATE INDEX VM_APP_PRVLG_K_PARENT ON VM_APP_PRIVILEGE (PARENT ASC);
CREATE INDEX VM_APP_PRVLG_K_TYPE ON VM_APP_PRIVILEGE (TYPE ASC);

-----------------------------------------------------------------------------------------------------------------------------------------------------------------------
-- Classification économique - NEC                                                                                                                                   --
-----------------------------------------------------------------------------------------------------------------------------------------------------------------------

-- Nature de dépense
DROP MATERIALIZED VIEW "ACTEUR".VM_APP_NATURE_DEPENSE;
CREATE MATERIALIZED VIEW "ACTEUR".VM_APP_NATURE_DEPENSE
REFRESH ON COMMIT 
COMPLETE
AS
SELECT 
    t.uuid AS "IDENTIFIANT"
    ,t.ndep_code AS "CODE"
    ,t.ndep_libct AS "LIBELLE"
FROM 
    NEC.nature_depense t;
--ALTER TABLE VM_APP_NATURE_DEPENSE ADD CONSTRAINT VM_APP_NATURE_DEPENSE_PK PRIMARY KEY (IDENTIFIANT);
ALTER TABLE VM_APP_NATURE_DEPENSE ADD CONSTRAINT VM_APP_NATURE_DEPENSE_UK_CODE UNIQUE (CODE);
-- Nature économique
DROP MATERIALIZED VIEW "ACTEUR".VM_APP_NATURE_ECONOMIQUE;
CREATE MATERIALIZED VIEW "ACTEUR".VM_APP_NATURE_ECONOMIQUE
REFRESH ON COMMIT 
COMPLETE
AS
SELECT n.uuid AS "IDENTIFIANT",n.nat_code AS "CODE",n.nat_liblg AS "LIBELLE"
FROM NEC.nature_economique n,NEC.TABLE_REFERENTIEL tref,NEC.VERSION_REFERENTIEL vref
WHERE n.nat_tref = tref.uuid (+) AND tref.tref_vers_id = vref.uuid AND vref.VERS_CODE='312' AND n.nat_imputable=1 
AND LENGTH(n.nat_code) = 6  /*AND n.nat_nat is null*/;
--ALTER TABLE VM_APP_NATURE_DEPENSE ADD CONSTRAINT VM_APP_NATURE_DEPENSE_PK PRIMARY KEY (IDENTIFIANT);
ALTER TABLE VM_APP_NATURE_ECONOMIQUE ADD CONSTRAINT VM_APP_NATURE_ECO_UK_CODE UNIQUE (CODE);

-- Domaine
DROP MATERIALIZED VIEW "ACTEUR".VM_APP_DOMAINE;
CREATE MATERIALIZED VIEW "ACTEUR".VM_APP_DOMAINE
TABLESPACE USERS
--REFRESH ON COMMIT 
REFRESH NEXT SYSDATE + 1/24
COMPLETE
AS
SELECT identifiant,code,libelle,'SECTION' AS "TYPE" FROM ACTEUR.VM_APP_SECTION
UNION ALL SELECT identifiant,code,libelle,'UA' AS "TYPE" FROM ACTEUR.VM_APP_UNITE_ADMINISTRATIVE
UNION ALL SELECT identifiant,code,libelle,'CATEGORIE_BUDGET' AS "TYPE" FROM ACTEUR.VM_APP_CAT_BUDGET
UNION ALL SELECT identifiant,code,libelle,'USB' AS "TYPE" FROM ACTEUR.VM_APP_USB
UNION ALL SELECT identifiant,code,libelle,'ACTION' AS "TYPE" FROM ACTEUR.VM_APP_ACTION
UNION ALL SELECT identifiant,code,libelle,'CATEGORIE_ACTIVITE' AS "TYPE" FROM ACTEUR.VM_APP_CATEG_ATV
UNION ALL SELECT identifiant,code,libelle,'ACTIVITE' AS "TYPE" FROM ACTEUR.VM_APP_ACTIVITE
UNION ALL SELECT identifiant,code,libelle,'IMPUTATION' AS "TYPE" FROM ACTEUR.VM_APP_IMPUTATION
UNION ALL SELECT identifiant,code,libelle,'LOCALITE' AS "TYPE" FROM ACTEUR.VM_APP_LOCALITE
UNION ALL SELECT identifiant,code,libelle,'SERVICE_ORD' AS "TYPE" FROM ACTEUR.SERVICE_ORD
UNION ALL SELECT identifiant,code,libelle,'SERVICE_CF' AS "TYPE" FROM ACTEUR.SERVICE_CF
UNION ALL SELECT identifiant,code,libelle,'SERVICE_CPT' AS "TYPE" FROM ACTEUR.SERVICE_CPT
;
ALTER TABLE VM_APP_DOMAINE ADD CONSTRAINT VM_APP_DOMAINE_PK PRIMARY KEY (IDENTIFIANT);
--ALTER TABLE VM_APP_DOMAINE ADD CONSTRAINT VM_APP_DOMAINE_UK_CODE UNIQUE (CODE);
CREATE INDEX VM_APP_DOMAINE_K_TYPE ON VM_APP_DOMAINE (TYPE ASC);

-----------------------------------------------------------------------------------------------------------------------------------------------------------------------
-- Exécution - BIDF                                                                                                                                                  --
-----------------------------------------------------------------------------------------------------------------------------------------------------------------------

-- Imputation Execution
DROP MATERIALIZED VIEW "ACTEUR".VM_APP_EX_IMPUTATION;
CREATE MATERIALIZED VIEW "ACTEUR".VM_APP_EX_IMPUTATION
TABLESPACE USERS
-- REFRESH ON COMMIT
REFRESH NEXT SYSDATE + 1/24 COMPLETE AS
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
    ,'LOCALITE'||(CASE WHEN LENGTH(l.loc_code) = 4 THEN (SELECT p.uuid FROM CA.LOCALITE p WHERE p.loc_code = SUBSTR(l.loc_code,1,LENGTH(l.loc_code)-2))
	WHEN LENGTH(l.loc_code) = 6 THEN (SELECT p.uuid FROM CA.LOCALITE p WHERE p.loc_code = SUBSTR(l.loc_code,1,LENGTH(l.loc_code)-4))
	ELSE NULL END) AS "REGION_IDENTIFIANT"	
    ,'LOCALITE'||(CASE WHEN LENGTH(l.loc_code) = 6 THEN (SELECT p.uuid FROM CA.LOCALITE p WHERE p.loc_code = SUBSTR(l.loc_code,1,LENGTH(l.loc_code)-2))
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
    ,CPP.categorie_budget cb
    ,ADS.activite_de_service adp
    ,NEC.nature_economique nec
    ,NEC.nature_depense nd
    ,CPP.action a
    ,ADS.categorie_activite ca    
    ,CPP.usb u
    ,CA.section_budgetaire s
    ,CA.unite_administrative ua
    ,CA.unite_administrative gestionnaire
    ,CA.localite l 
    ,CA.localite localite_gestionnaire
    ,CA.localite localite_activite
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
    ;
ALTER TABLE VM_APP_EX_IMPUTATION ADD CONSTRAINT VM_APP_EX_IMPUTATION_PK PRIMARY KEY (IDENTIFIANT);
CREATE INDEX VM_APP_EX_IMPUTATION_K_EXO ON VM_APP_EX_IMPUTATION (EXERCICE ASC);
CREATE INDEX VM_APP_EX_IMPUTATION_K_SECTION ON VM_APP_EX_IMPUTATION (SECTION ASC);
CREATE INDEX VM_APP_EX_IMPUTATION_K_S_CL ON VM_APP_EX_IMPUTATION (SECTION_CODE_LIBELLE ASC);
CREATE INDEX VM_APP_EX_IMPUTATION_K_UA ON VM_APP_EX_IMPUTATION (UA ASC);
CREATE INDEX VM_APP_EX_IMPUTATION_K_UA_CL ON VM_APP_EX_IMPUTATION (UA_CODE_LIBELLE ASC);
CREATE INDEX VM_APP_EX_IMPUTATION_K_USB ON VM_APP_EX_IMPUTATION (USB ASC);
CREATE INDEX VM_APP_EX_IMPUTATION_K_USB_CL ON VM_APP_EX_IMPUTATION (USB_CODE_LIBELLE ASC);
CREATE INDEX VM_APP_EX_IMPUTATION_K_ACTION ON VM_APP_EX_IMPUTATION (ACTION ASC);
CREATE INDEX VM_APP_EX_IMPUTATION_K_A_CL ON VM_APP_EX_IMPUTATION (ACTION_CODE_LIBELLE ASC);
CREATE INDEX VM_APP_EX_IMPUTATION_K_ACTV ON VM_APP_EX_IMPUTATION (ACTIVITE ASC);
CREATE INDEX VM_APP_EX_IMPUTATION_K_ACTV_CL ON VM_APP_EX_IMPUTATION (ACTIVITE_CODE_LIBELLE ASC);
CREATE INDEX VM_APP_EX_IMPUTATION_K_NEC ON VM_APP_EX_IMPUTATION (NATURE_ECONOMIQUE ASC);
CREATE INDEX VM_APP_EX_IMPUTATION_K_NEC_CL ON VM_APP_EX_IMPUTATION (NATURE_ECONOMIQUE_CODE_LIBELLE ASC);

-- Mapping Ancien et Nouveau codes 2021
DROP MATERIALIZED VIEW VMT_AFF_IMP_LIEN;
CREATE MATERIALIZED VIEW VMT_AFF_IMP_LIEN
TABLESPACE USERS REFRESH NEXT SYSDATE + 1/24 COMPLETE AS
SELECT a.identifiant,SUBSTR(i.activite,LENGTH('ACTIVITE')+1)||i.nature_economique AS "ANCIEN",i.exercice||SUBSTR(i.activite,LENGTH('ACTIVITE')+1)||i.nature_economique AS "NOUVEAU"
FROM AFFECTATIONS a
JOIN VM_APP_EX_IMPUTATION i ON i.ldep_id = a.identifiant
WHERE i.exercice = 2021;
ALTER TABLE VMT_AFF_IMP_LIEN ADD CONSTRAINT VMT_AFF_IMP_LIEN_PK PRIMARY KEY (IDENTIFIANT);
CREATE INDEX VMT_AFF_IMP_LIEN_K_ANCIEN ON VMT_AFF_IMP_LIEN (ANCIEN ASC);
CREATE INDEX VMT_AFF_IMP_LIEN_K_NOUVEAU ON VMT_AFF_IMP_LIEN (NOUVEAU ASC);