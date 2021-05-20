-- Section
DROP MATERIALIZED VIEW "SIIBC_ACTOR".VM_APP_SECTION;
CREATE MATERIALIZED VIEW "SIIBC_ACTOR".VM_APP_SECTION
REFRESH ON COMMIT 
COMPLETE
AS
SELECT 'SECTION'||s.uuid AS "IDENTIFIANT",s.secb_code AS "CODE",s.secb_libelle AS "LIBELLE"
FROM SIIBC_CA.section_budgetaire s,SIIBC_CA.gouvernement g
WHERE s.gouv_id = g.uuid AND g.gouv_statut = 'ENABLED' AND g.gouv_utilise = 1 AND s.entitystatus = 'COMMITTED';
ALTER TABLE VM_APP_SECTION ADD CONSTRAINT VM_APP_SECTION_PK PRIMARY KEY (IDENTIFIANT);
ALTER TABLE VM_APP_SECTION ADD CONSTRAINT VM_APP_SECTION_UK_CODE UNIQUE (CODE);
-- Unit� de sp�cialisation du budget
DROP MATERIALIZED VIEW "SIIBC_ACTOR".VM_APP_USB;
CREATE MATERIALIZED VIEW "SIIBC_ACTOR".VM_APP_USB
REFRESH ON COMMIT 
COMPLETE
AS
SELECT
    'USB'||usb.uuid AS "IDENTIFIANT"
    ,usb.usb_code AS "CODE"
    ,usb.usb_liblg AS "LIBELLE"
    ,'SECTION'||s.uuid AS "SECTION"
    ,s.secb_code||' '||s.secb_libelle AS "SECTION_CODE_LIBELLE"
FROM SIIBC_CPP.usb usb,SIIBC_CA.section_budgetaire s
WHERE s.uuid = usb.usb_secb_id AND s.entitystatus = 'COMMITTED';
ALTER TABLE VM_APP_USB ADD CONSTRAINT VM_APP_USB_PK PRIMARY KEY (IDENTIFIANT);
ALTER TABLE VM_APP_USB ADD CONSTRAINT VM_APP_USB_UK_CODE UNIQUE (CODE);
CREATE INDEX VM_APP_USB_K_SECTION ON VM_APP_USB (SECTION ASC);
-- Action
DROP MATERIALIZED VIEW "SIIBC_ACTOR".VM_APP_ACTION;
CREATE MATERIALIZED VIEW "SIIBC_ACTOR".VM_APP_ACTION
REFRESH ON COMMIT 
COMPLETE
AS
SELECT
    'ACTION'||a.uuid AS "IDENTIFIANT"
    ,a.adp_code AS "CODE"
    ,a.adp_liblg AS "LIBELLE"
    ,'SECTION'||s.uuid AS "SECTION"
    ,s.secb_code||' '||s.secb_libelle AS "SECTION_CODE_LIBELLE"
    ,'USB'||usb.uuid AS "USB"
    ,usb.usb_code||' '||usb.usb_liblg AS "USB_CODE_LIBELLE"
FROM SIIBC_CPP.action a,SIIBC_CPP.usb usb,SIIBC_CA.section_budgetaire s
WHERE a.adp_usb_id = usb.uuid AND s.uuid = usb.usb_secb_id;
ALTER TABLE VM_APP_ACTION ADD CONSTRAINT VM_APP_ACTION_PK PRIMARY KEY (IDENTIFIANT);
ALTER TABLE VM_APP_ACTION ADD CONSTRAINT VM_APP_ACTION_UK_CODE UNIQUE (CODE);
CREATE INDEX VM_APP_ACTION_K_SECTION ON VM_APP_ACTION (SECTION ASC);
CREATE INDEX VM_APP_ACTION_K_USB ON VM_APP_ACTION (USB ASC);
-- Cat�gorie Activit�
DROP MATERIALIZED VIEW "SIIBC_ACTOR".VM_APP_CATEG_ATV;
CREATE MATERIALIZED VIEW "SIIBC_ACTOR".VM_APP_CATEG_ATV
REFRESH ON COMMIT 
COMPLETE
AS
SELECT 'CATEGORIE_ACTIVITE'||ca.catv_id AS "IDENTIFIANT",ca.catv_code AS "CODE",ca.catv_liblg AS "LIBELLE"
FROM SIIBC_ADS.categorie_activite ca;
ALTER TABLE VM_APP_CATEG_ATV ADD CONSTRAINT VM_APP_CATEG_ATV_PK PRIMARY KEY (IDENTIFIANT);
ALTER TABLE VM_APP_CATEG_ATV ADD CONSTRAINT VM_APP_CATEG_ATV_UK_CODE UNIQUE (CODE);
-- Activit�
DROP MATERIALIZED VIEW "SIIBC_ACTOR".VM_APP_ACTIVITE;
CREATE MATERIALIZED VIEW "SIIBC_ACTOR".VM_APP_ACTIVITE
REFRESH ON COMMIT 
COMPLETE
AS
SELECT 'ACTIVITE'||a.ads_id AS "IDENTIFIANT"
    ,a.ads_code AS "CODE"
    ,a.ads_liblg AS "LIBELLE"
    ,'CATEGORIE_ACTIVITE'||c.catv_id AS "CATEGORIE"
    ,c.catv_code||' '||c.catv_liblg AS "CATEGORIE_CODE_LIBELLE"
    ,'ACTION'||action.uuid AS "ACTION"
    ,action.adp_code||' '||action.adp_liblg AS "ACTION_CODE_LIBELLE"
    ,'USB'||usb.uuid AS "USB"
    ,usb.usb_code||' '||usb.usb_liblg AS "USB_CODE_LIBELLE"
    ,'SECTION'||section.uuid AS "SECTION"
    ,section.secb_code||' '||section.secb_libelle AS "SECTION_CODE_LIBELLE"
    ,nd.uuid AS "NATURE_DEPENSE"
    ,nd.ndep_code||' '||nd.ndep_libct AS "NATURE_DEPENSE_CODE_LIBELLE"
    ,CASE WHEN ua.uuid IS NULL THEN NULL ELSE 'UA'||ua.uuid END AS "UA"
    ,CASE WHEN ua.ua_code IS NULL THEN NULL ELSE ua.ua_code||' '||ua.ua_liblg END AS "UA_CODE_LIBELLE"
FROM SIIBC_ADS.activite_de_service a,SIIBC_ADS.categorie_activite c,SIIBC_CPP.action action,SIIBC_CPP.usb usb
,SIIBC_CA.section_budgetaire section,SIIBC_NEC.nature_depense nd,SIIBC_CA.unite_administrative ua
WHERE a.catv_id = c.catv_id (+) AND a.adp_id = action.uuid (+) AND action.adp_usb_id = usb.uuid (+) 
AND usb.usb_secb_id = section.uuid (+) AND nd.ndep_code = a.ndep_id (+) AND ua.uuid (+) = a.ua_benef_id;
ALTER TABLE VM_APP_ACTIVITE ADD CONSTRAINT VM_APP_ACTIVITE_PK PRIMARY KEY (IDENTIFIANT);
ALTER TABLE VM_APP_ACTIVITE ADD CONSTRAINT VM_APP_ACTIVITE_UK_CODE UNIQUE (CODE);
CREATE INDEX VM_APP_ACTIVITE_K_SECTION ON VM_APP_ACTIVITE (SECTION ASC);
CREATE INDEX VM_APP_ACTIVITE_K_USB ON VM_APP_ACTIVITE (USB ASC);
CREATE INDEX VM_APP_ACTIVITE_K_ACTION ON VM_APP_ACTIVITE (ACTION ASC);
CREATE INDEX VM_APP_ACTIVITE_K_ND ON VM_APP_ACTIVITE (NATURE_DEPENSE ASC);
CREATE INDEX VM_APP_ACTIVITE_K_UA ON VM_APP_ACTIVITE (UA ASC);
-- Imputation
DROP MATERIALIZED VIEW "SIIBC_ACTOR".VM_APP_IMPUTATION;
CREATE MATERIALIZED VIEW "SIIBC_ACTOR".VM_APP_IMPUTATION
TABLESPACE USERS
-- REFRESH ON COMMIT
REFRESH NEXT SYSDATE + 1/48 COMPLETE
AS
SELECT
    ld.uuid AS "LDEP_ID"
    ,ld.dep_activite_id||ld.dep_nature_eco_id AS "IDENTIFIANT"
    ,adp.ads_code||nec.nat_code AS "CODE"
    ,adp.ads_liblg||' | '||nec.nat_liblg AS "LIBELLE"
    ,'ACTIVITE'||adp.ads_id AS "ACTIVITE"
    ,'ACTIVITE'||adp.ads_id AS "ACTIVITE_IDENTIFIANT"
    ,adp.ads_code AS "ACTIVITE_CODE"
    ,adp.ads_code||' '||adp.ads_liblg AS "ACTIVITE_CODE_LIBELLE"
    ,nec.uuid AS "NATURE_ECONOMIQUE"
    ,nec.uuid AS "NATURE_ECONOMIQUE_IDENTIFIANT"
    ,nec.nat_code AS "NATURE_ECONOMIQUE_CODE"
    ,nec.nat_code||' '||nec.nat_liblg AS "NATURE_ECONOMIQUE_CODE_LIBELLE"
    ,'ACTION'||a.uuid AS "ACTION"
    ,'ACTION'||a.uuid AS "ACTION_IDENTIFIANT"
    ,a.adp_code AS "ACTION_CODE"
    ,a.adp_code||' '||a.adp_liblg AS "ACTION_CODE_LIBELLE"
    ,'USB'||u.uuid AS "USB"
    ,'USB'||u.uuid AS "USB_IDENTIFIANT"
    ,u.usb_code AS "USB_CODE"
    ,u.usb_code||' '||u.usb_liblg AS "USB_CODE_LIBELLE"
    ,'SECTION'||s.uuid AS "SECTION"
    ,'SECTION'||s.uuid AS "SECTION_IDENTIFIANT"
    ,s.secb_code AS "SECTION_CODE"
    ,s.secb_code||' '||s.secb_libelle AS "SECTION_CODE_LIBELLE"
    ,'UA'||ua.uuid AS "UA"
    ,'UA'||ua.uuid AS "UA_IDENTIFIANT"
    ,ua.ua_code AS "UA_CODE"
    ,ua.ua_code||' '||ua.ua_liblg AS "UA_CODE_LIBELLE"
    ,'UA'||ua.uuid AS "GESTIONNAIRE"
    ,gestionnaire.ua_code AS "GESTIONNAIRE_CODE"
    ,gestionnaire.ua_code||' '||ua.ua_liblg AS "GESTIONNAIRE_CODE_LIBELLE"
    ,l.loc_code AS "UA_LOCALITE_CODE"
    ,'CATEGORIE_ACTIVITE'||ca.catv_id AS "CATEGORIE_ACTIVITE"
    ,'CATEGORIE_ACTIVITE'||ca.catv_id AS "CA_IDENTIFIANT"
    ,ca.catv_code AS "CA_CODE"
    ,ca.catv_code||' '||ca.catv_liblg AS "CA_CODE_LIBELLE"
    ,nd.uuid AS "ND_IDENTIFIANT" 
    ,nd.ndep_code AS "ND_CODE" 
    ,nd.ndep_code||' '||nd.ndep_liblg AS "ND_CODE_LIBELLE" 
    ,'LOCALITE'||(CASE WHEN LENGTH(l.loc_code) = 4 THEN (SELECT p.uuid FROM SIIBC_CA.LOCALITE p WHERE p.loc_code = SUBSTR(l.loc_code,1,LENGTH(l.loc_code)-2))
	WHEN LENGTH(l.loc_code) = 6 THEN (SELECT p.uuid FROM SIIBC_CA.LOCALITE p WHERE p.loc_code = SUBSTR(l.loc_code,1,LENGTH(l.loc_code)-4))
	ELSE NULL END) AS "REGION_IDENTIFIANT"	
    ,'LOCALITE'||(CASE WHEN LENGTH(l.loc_code) = 6 THEN (SELECT p.uuid FROM SIIBC_CA.LOCALITE p WHERE p.loc_code = SUBSTR(l.loc_code,1,LENGTH(l.loc_code)-2))
	ELSE NULL END) AS "DEPARTEMENT_IDENTIFIANT"	
    ,'LOCALITE'||l.uuid AS "SOUS_PREFECTURE_IDENTIFIANT"
    ,localite_gestionnaire.loc_code AS "GESTIONNAIRE_LOCALITE_CODE"
    ,localite_activite.loc_code AS "ACTIVITE_LOCALITE_CODE"
FROM
    SIIBC_BUDGET.ligne_depense ld
    ,SIIBC_ADS.activite_de_service adp
    ,SIIBC_NEC.nature_economique nec
    ,SIIBC_NEC.nature_depense nd
    ,SIIBC_CPP.action a
    ,SIIBC_ADS.categorie_activite ca    
    ,SIIBC_CPP.usb u
    ,SIIBC_CA.section_budgetaire s
    ,SIIBC_CA.unite_administrative ua
    ,SIIBC_CA.localite l
    ,SIIBC_CA.unite_administrative gestionnaire
    ,SIIBC_CA.localite localite_gestionnaire
    ,SIIBC_CA.localite localite_activite
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
-- Unit� administrative
DROP MATERIALIZED VIEW "SIIBC_ACTOR".VM_APP_UNITE_ADMINISTRATIVE;
CREATE MATERIALIZED VIEW "SIIBC_ACTOR".VM_APP_UNITE_ADMINISTRATIVE
REFRESH ON COMMIT 
COMPLETE
AS
SELECT
    'UA'||ua.uuid AS "IDENTIFIANT"
    ,ua.ua_code AS "CODE"
    ,ua.ua_liblg AS "LIBELLE"
    ,CASE WHEN s.uuid IS NULL OR s.entitystatus <> 'COMMITTED' THEN NULL ELSE 'SECTION'||s.uuid END AS "SECTION"
    ,CASE WHEN s.uuid IS NULL OR s.entitystatus <> 'COMMITTED' THEN NULL ELSE s.secb_code||' '||s.secb_libelle END AS "SECTION_CODE_LIBELLE"
    
    ,CASE WHEN l.uuid IS NULL OR l.entitystatus <> 'COMMITTED' THEN NULL ELSE 'LOCALITE'||l.uuid END AS "LOCALITE"
    ,CASE WHEN l.uuid IS NULL OR l.entitystatus <> 'COMMITTED' THEN NULL ELSE 'LOCALITE'||l.uuid END AS "LOCALITE_IDENTIFIANT"
    ,CASE WHEN l.uuid IS NULL OR l.entitystatus <> 'COMMITTED' THEN NULL ELSE l.loc_code END AS "LOCALITE_CODE"
    ,CASE WHEN l.uuid IS NULL OR l.entitystatus <> 'COMMITTED' THEN NULL ELSE l.loc_code||' '||l.loc_lib END AS "LOCALITE_CODE_LIBELLE"
FROM SIIBC_CA.unite_administrative ua,SIIBC_CA.section_budgetaire s,SIIBC_CA.localite l
WHERE s.uuid (+) = ua.ua_secb_id AND l.uuid (+) = ua.ua_loc_id;
ALTER TABLE VM_APP_UNITE_ADMINISTRATIVE ADD CONSTRAINT VM_APP_UA_PK PRIMARY KEY (IDENTIFIANT);
ALTER TABLE VM_APP_UNITE_ADMINISTRATIVE ADD CONSTRAINT VM_APP_UA_UK_CODE UNIQUE (CODE);
CREATE INDEX VM_APP_UA_K_SECTION ON VM_APP_UNITE_ADMINISTRATIVE (SECTION ASC);
CREATE INDEX VM_APP_UA_K_LOCALITE ON VM_APP_UNITE_ADMINISTRATIVE (LOCALITE ASC);
-- Localit�
DROP MATERIALIZED VIEW "SIIBC_ACTOR".VM_APP_LOCALITE;
CREATE MATERIALIZED VIEW "SIIBC_ACTOR".VM_APP_LOCALITE
REFRESH NEXT SYSDATE + 1/48 COMPLETE AS
SELECT 'LOCALITE'||l.uuid AS "IDENTIFIANT",l.loc_code AS "CODE",l.loc_lib AS "LIBELLE"
,CASE WHEN LENGTH(l.loc_code) = 2 AND (LOWER(l.loc_lib) LIKE 'region%' OR LOWER(l.loc_lib) LIKE 'r�gion%') THEN 'REGION' 
WHEN LENGTH(l.loc_code) = 4 THEN 'DEPARTEMENT' ELSE 'SOUS_PREFECTURE' END AS "TYPE"
,(CASE WHEN LENGTH(l.loc_code) = 2 AND (LOWER(l.loc_lib) LIKE 'region%' OR LOWER(l.loc_lib) LIKE 'r�gion%') THEN NULL 
WHEN LENGTH(l.loc_code) >=4  THEN (SELECT 'LOCALITE'||p.uuid FROM SIIBC_CA.LOCALITE p WHERE p.loc_code = SUBSTR(l.loc_code,1,LENGTH(l.loc_code)-2))
--WHEN LENGTH(l.loc_code) = 4 THEN (SELECT 'LOCALITE'||p.uuid FROM SIIBC_CA.LOCALITE p WHERE p.loc_code = SUBSTR(l.loc_code,1,2))
--WHEN LENGTH(l.loc_code) = 6 THEN (SELECT 'LOCALITE'||p.uuid FROM SIIBC_CA.LOCALITE p WHERE p.loc_code = SUBSTR(l.loc_code,1,4))
ELSE NULL END) AS "PARENT"
-- D�partement
,(CASE WHEN LENGTH(l.loc_code) = 6 THEN 'LOCALITE'||(SELECT p.uuid FROM SIIBC_CA.LOCALITE p WHERE p.loc_code = SUBSTR(l.loc_code,1,LENGTH(l.loc_code)-2))
ELSE NULL END) AS "DEPARTEMENT_IDENTIFIANT"
,(CASE WHEN LENGTH(l.loc_code) = 6 THEN (SELECT p.loc_code||' '||p.loc_lib FROM SIIBC_CA.LOCALITE p WHERE p.loc_code = SUBSTR(l.loc_code,1,LENGTH(l.loc_code)-2))
ELSE NULL END) AS "DEPARTEMENT_CODE_LIBELLE"
-- R�gion
,(CASE WHEN LENGTH(l.loc_code) = 4 THEN 'LOCALITE'||(SELECT p.uuid FROM SIIBC_CA.LOCALITE p WHERE p.loc_code = SUBSTR(l.loc_code,1,LENGTH(l.loc_code)-2))
WHEN LENGTH(l.loc_code) = 6 THEN 'LOCALITE'||(SELECT p.uuid FROM SIIBC_CA.LOCALITE p WHERE p.loc_code = SUBSTR(l.loc_code,1,LENGTH(l.loc_code)-4))
ELSE NULL END) AS "REGION_IDENTIFIANT"
,(CASE WHEN LENGTH(l.loc_code) = 4 THEN (SELECT p.loc_code||' '||p.loc_lib FROM SIIBC_CA.LOCALITE p WHERE p.loc_code = SUBSTR(l.loc_code,1,LENGTH(l.loc_code)-2))
WHEN LENGTH(l.loc_code) = 6 THEN (SELECT p.loc_code||' '||p.loc_lib FROM SIIBC_CA.LOCALITE p WHERE p.loc_code = SUBSTR(l.loc_code,1,LENGTH(l.loc_code)-4))
ELSE NULL END) AS "REGION_CODE_LIBELLE"
FROM SIIBC_CA.localite l WHERE l.entitystatus = 'COMMITTED';
ALTER TABLE VM_APP_LOCALITE ADD CONSTRAINT VM_APP_LOCALITE_PK PRIMARY KEY (IDENTIFIANT);
ALTER TABLE VM_APP_LOCALITE ADD CONSTRAINT VM_APP_LOCALITE_UK_CODE UNIQUE (CODE);
CREATE INDEX VM_APP_LOCALITE_K_PARENT ON VM_APP_LOCALITE (PARENT ASC);
-- Domaine
DROP MATERIALIZED VIEW "SIIBC_ACTOR".VM_APP_DOMAINE;
CREATE MATERIALIZED VIEW "SIIBC_ACTOR".VM_APP_DOMAINE
TABLESPACE USERS
--REFRESH ON COMMIT 
REFRESH NEXT SYSDATE + 1/24
COMPLETE
AS
SELECT identifiant,code,libelle,'SECTION' AS "TYPE" FROM SIIBC_ACTOR.VM_APP_SECTION
UNION ALL SELECT identifiant,code,libelle,'USB' AS "TYPE" FROM SIIBC_ACTOR.VM_APP_USB
UNION ALL SELECT identifiant,code,libelle,'ACTION' AS "TYPE" FROM SIIBC_ACTOR.VM_APP_ACTION
UNION ALL SELECT identifiant,code,libelle,'CATEGORIE_ACTIVITE' AS "TYPE" FROM SIIBC_ACTOR.VM_APP_CATEG_ATV
UNION ALL SELECT identifiant,code,libelle,'ACTIVITE' AS "TYPE" FROM SIIBC_ACTOR.VM_APP_ACTIVITE
UNION ALL SELECT identifiant,code,libelle,'IMPUTATION' AS "TYPE" FROM SIIBC_ACTOR.VM_APP_IMPUTATION
UNION ALL SELECT identifiant,code,libelle,'UA' AS "TYPE" FROM SIIBC_ACTOR.VM_APP_UNITE_ADMINISTRATIVE
UNION ALL SELECT identifiant,code,libelle,'LOCALITE' AS "TYPE" FROM SIIBC_ACTOR.VM_APP_LOCALITE
UNION ALL SELECT identifiant,code,libelle,'SERVICE_ORD' AS "TYPE" FROM SIIBC_ACTOR.SERVICE_ORD
UNION ALL SELECT identifiant,code,libelle,'SERVICE_CF' AS "TYPE" FROM SIIBC_ACTOR.SERVICE_CF
UNION ALL SELECT identifiant,code,libelle,'SERVICE_CPT' AS "TYPE" FROM SIIBC_ACTOR.SERVICE_CPT
;
ALTER TABLE VM_APP_DOMAINE ADD CONSTRAINT VM_APP_DOMAINE_PK PRIMARY KEY (IDENTIFIANT);
--ALTER TABLE VM_APP_DOMAINE ADD CONSTRAINT VM_APP_DOMAINE_UK_CODE UNIQUE (CODE);
CREATE INDEX VM_APP_DOMAINE_K_TYPE ON VM_APP_DOMAINE (TYPE ASC);
-- Nature de d�pense
DROP MATERIALIZED VIEW "SIIBC_ACTOR".VM_APP_NATURE_DEPENSE;
CREATE MATERIALIZED VIEW "SIIBC_ACTOR".VM_APP_NATURE_DEPENSE
REFRESH ON COMMIT 
COMPLETE
AS
SELECT 
    t.uuid AS "IDENTIFIANT"
    ,t.ndep_code AS "CODE"
    ,t.ndep_libct AS "LIBELLE"
FROM 
    SIIBC_NEC.nature_depense t;
--ALTER TABLE VM_APP_NATURE_DEPENSE ADD CONSTRAINT VM_APP_NATURE_DEPENSE_PK PRIMARY KEY (IDENTIFIANT);
ALTER TABLE VM_APP_NATURE_DEPENSE ADD CONSTRAINT VM_APP_NATURE_DEPENSE_UK_CODE UNIQUE (CODE);
-- Nature �conomique
DROP MATERIALIZED VIEW "SIIBC_ACTOR".VM_APP_NATURE_ECONOMIQUE;
CREATE MATERIALIZED VIEW "SIIBC_ACTOR".VM_APP_NATURE_ECONOMIQUE
REFRESH ON COMMIT 
COMPLETE
AS
SELECT n.uuid AS "IDENTIFIANT",n.nat_code AS "CODE",n.nat_liblg AS "LIBELLE"
FROM SIIBC_NEC.nature_economique n,SIIBC_NEC.TABLE_REFERENTIEL tref,SIIBC_NEC.VERSION_REFERENTIEL vref
WHERE n.nat_tref = tref.uuid (+) AND tref.tref_vers_id = vref.uuid AND vref.VERS_CODE='312' AND n.nat_imputable=1 AND n.nat_nat is null;
--ALTER TABLE VM_APP_NATURE_DEPENSE ADD CONSTRAINT VM_APP_NATURE_DEPENSE_PK PRIMARY KEY (IDENTIFIANT);
ALTER TABLE VM_APP_NATURE_ECONOMIQUE ADD CONSTRAINT VM_APP_NATURE_ECO_UK_CODE UNIQUE (CODE);
-- Module
DROP MATERIALIZED VIEW "SIIBC_ACTOR".VM_APP_MODULE;
CREATE MATERIALIZED VIEW "SIIBC_ACTOR".VM_APP_MODULE
REFRESH ON COMMIT 
COMPLETE
AS
SELECT 
    'MODULE'||t.id AS "IDENTIFIANT"
    ,t.id AS "CODE"
    ,t.name AS "LIBELLE"
FROM 
    SIIBC_PRT.msvc_module t;
--ALTER TABLE VM_APP_MODULE ADD CONSTRAINT VM_APP_MODULE_PK PRIMARY KEY (IDENTIFIANT);
--ALTER TABLE VM_APP_MODULE ADD CONSTRAINT VM_APP_MODULE_UK_CODE UNIQUE (CODE);
-- Service
DROP MATERIALIZED VIEW "SIIBC_ACTOR".VM_APP_SERVICE;
CREATE MATERIALIZED VIEW "SIIBC_ACTOR".VM_APP_SERVICE
REFRESH ON COMMIT 
COMPLETE
AS
SELECT 
    'SERVICE'||t.id AS "IDENTIFIANT"
    ,CASE WHEN t.url = '/acteur/private/index.jsf' THEN 'mic-acteur' ELSE t.url END AS "CODE"
    --,t.url AS "CODE"
    ,t.name AS "LIBELLE"
    ,'MODULE'||t.id_module AS "MODULE"
    ,m.code||' '||m.name AS "MODULE_CODE_LIBELLE"
    ,COUNT(me.id) AS "NOMBRE_DE_MENUS"
FROM 
    SIIBC_PRT.msvc_service t,SIIBC_PRT.msvc_module m,SIIBC_PRT.msvc_menu me
WHERE
    t.id_module = m.id AND t.id = me.id_service
GROUP BY
    'SERVICE'||t.id,CASE WHEN t.url = '/acteur/private/index.jsf' THEN 'mic-acteur' ELSE t.url END,t.name,'MODULE'||t.id_module,m.code||' '||m.name;
ALTER TABLE VM_APP_SERVICE ADD CONSTRAINT VM_APP_SERVICE_PK PRIMARY KEY (IDENTIFIANT);
--ALTER TABLE VM_APP_SERVICE ADD CONSTRAINT VM_APP_SERVICE_UK_CODE UNIQUE (CODE);
-- Menu
DROP MATERIALIZED VIEW "SIIBC_ACTOR".VM_APP_MENU;
CREATE MATERIALIZED VIEW "SIIBC_ACTOR".VM_APP_MENU
REFRESH ON COMMIT 
COMPLETE
AS
SELECT
    'MENU'||t.id AS "IDENTIFIANT"
    ,t.code AS "CODE"
    ,t.name AS "LIBELLE"
    ,t.url AS "URL"
    ,'SERVICE'||t.id_service AS "SERVICE"
    ,s.code||' '||s.name AS "SERVICE_CODE_LIBELLE"
    ,m.code||' '||m.name AS "MODULE_CODE_LIBELLE"
    --,CASE WHEN t.menuparent_id IS NULL THEN 'SERVICE'||t.id_service ELSE 'MENU'||t.menuparent_id END AS "PARENT"
FROM
    SIIBC_PRT.msvc_menu t,SIIBC_PRT.msvc_service s,SIIBC_PRT.msvc_module m
WHERE
    t.id_service = s.id AND s.id_module = m.id AND t.abstrait = 0;
ALTER TABLE VM_APP_MENU ADD CONSTRAINT VM_APP_MENU_PK PRIMARY KEY (IDENTIFIANT);
ALTER TABLE VM_APP_MENU ADD CONSTRAINT VM_APP_MENU_UK_CODE UNIQUE (CODE);
-- Privil�ge
DROP MATERIALIZED VIEW "SIIBC_ACTOR".VM_APP_PRIVILEGE;
CREATE MATERIALIZED VIEW "SIIBC_ACTOR".VM_APP_PRIVILEGE
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
    SIIBC_PRT.msvc_module t
UNION ALL
SELECT 
    'SERVICE'||t.id AS "IDENTIFIANT"
    ,t.code AS "CODE"
    ,t.name AS "LIBELLE"
    ,'MODULE'||t.id_module AS "PARENT"
    ,'SERVICE' AS "TYPE"
FROM 
    SIIBC_PRT.msvc_service t
UNION ALL
SELECT
    'MENU'||t.id AS "IDENTIFIANT"
    ,t.code AS "CODE"
    ,t.name AS "LIBELLE"
    --,'SERVICE'||t.id_service AS "PARENT"
    ,CASE WHEN t.menuparent_id IS NULL THEN 'SERVICE'||t.id_service ELSE 'MENU'||t.menuparent_id END AS "PARENT"
    ,'MENU' AS "TYPE"
FROM
    SIIBC_PRT.msvc_menu t
UNION ALL
SELECT
    'ACTION'||t.id AS "IDENTIFIANT"
    ,t.code AS "CODE"
    ,t.libelle AS "LIBELLE"
    ,'MENU'||t.id_menu AS "PARENT"
    ,'ACTION' AS "TYPE"
FROM
    SIIBC_PRT.msvc_action t
;
ALTER TABLE VM_APP_PRIVILEGE ADD CONSTRAINT VM_APP_PRVLG_PK PRIMARY KEY (IDENTIFIANT);
--ALTER TABLE VM_APP_PRIVILEGE ADD CONSTRAINT VM_APP_PRVLG_UK_CODE UNIQUE (CODE);
CREATE INDEX VM_APP_PRVLG_K_PARENT ON VM_APP_PRIVILEGE (PARENT ASC);
CREATE INDEX VM_APP_PRVLG_K_TYPE ON VM_APP_PRIVILEGE (TYPE ASC);
-- Imputation Execution
DROP MATERIALIZED VIEW "SIIBC_ACTOR".VM_APP_EX_IMPUTATION;
CREATE MATERIALIZED VIEW "SIIBC_ACTOR".VM_APP_EX_IMPUTATION
TABLESPACE USERS
-- REFRESH ON COMMIT
REFRESH NEXT SYSDATE + 1/24
COMPLETE
AS
SELECT
    ld.ldep_id AS "LDEP_ID"
    ,ld.exo_num||ld.ads_id||ld.nat_id AS "IDENTIFIANT"
    ,ld.exo_num||adp.ads_code||nec.nat_code AS "CODE"
    ,adp.ads_liblg||' | '||nec.nat_liblg AS "LIBELLE"
    ,ld.exo_num AS "EXERCICE"
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
    ,nd.uuid AS "ND_IDENTIFIANT" 
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
    SIIBC_MEA.ligne_de_depenses ld
    ,SIIBC_ADS.activite_de_service adp
    ,SIIBC_CPP.categorie_budget cb  
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
    ;
ALTER TABLE VM_APP_EX_IMPUTATION ADD CONSTRAINT VM_APP_EX_IMPUTATION_PK PRIMARY KEY (IDENTIFIANT);
ALTER TABLE VM_APP_EX_IMPUTATION PARALLEL 8;
--ALTER TABLE VM_APP_EX_IMPUTATION COMPRESS;
CREATE INDEX VM_APP_EX_IMPUTATION_K_SECTION ON VM_APP_EX_IMPUTATION (SECTION ASC);
CREATE INDEX VM_APP_EX_IMPUTATION_K_USB ON VM_APP_EX_IMPUTATION (USB ASC);
CREATE INDEX VM_APP_EX_IMPUTATION_K_ACTION ON VM_APP_EX_IMPUTATION (ACTION ASC);
CREATE INDEX VM_APP_EX_IMPUTATION_K_ACTV ON VM_APP_EX_IMPUTATION (ACTIVITE ASC);
CREATE INDEX VM_APP_EX_IMPUTATION_K_NEC ON VM_APP_EX_IMPUTATION (NATURE_ECONOMIQUE ASC);
CREATE INDEX VM_APP_EX_IMPUTATION_K_UA ON VM_APP_EX_IMPUTATION (UA ASC);
-- Views
CREATE OR REPLACE VIEW V_FONCTIONS_GC
AS SELECT
    p.identifiant AS "IDENTIFIANT"
    ,p.code AS "CODE"
    ,p.libelle AS "LIBELLE"
    ,SUBSTR(section.identifiant,8) AS "SECTION"
    ,SUBSTR(p.code,1,2) AS "TYPE"
    ,NULL AS "DOCUMENT"
    ,SUBSTR(localite.identifiant,9) AS "LOCALITE"
    ,SUBSTR(ua.identifiant,3) AS "CHAMP_ACTION"
FROM
    poste p
LEFT JOIN VM_APP_UNITE_ADMINISTRATIVE ua ON ua.identifiant = p.domaine
LEFT JOIN VM_APP_SECTION section ON section.identifiant = ua.section
LEFT JOIN VM_APP_LOCALITE localite ON localite.identifiant = ua.localite
LEFT JOIN FONCTION fonction ON fonction.identifiant = p.fonction
WHERE
    fonction.code = 'GC' OR fonction.code = 'AGC';

CREATE OR REPLACE VIEW V_FONCTIONS_ORD
AS SELECT
    p.identifiant AS "IDENTIFIANT"
    ,p.code AS "CODE"
    ,p.libelle AS "LIBELLE"
    ,SUBSTR(section.identifiant,8) AS "SECTION"
    ,SUBSTR(p.code,1,2) AS "TYPE"
    ,NULL AS "DOCUMENT"
    ,SUBSTR(ord.localite,9) AS "LOCALITE"
FROM
    poste p
LEFT JOIN SERVICE_ORD ord ON ord.identifiant = p.domaine
LEFT JOIN VM_APP_USB usb ON usb.identifiant = ord.usb
LEFT JOIN VM_APP_SECTION section ON section.identifiant = usb.section
LEFT JOIN FONCTION fonction ON fonction.identifiant = p.fonction
WHERE
    fonction.code = 'ORD' OR fonction.code = 'AORD';

CREATE OR REPLACE VIEW V_FONCTIONS_CF
AS SELECT
    p.identifiant AS "IDENTIFIANT"
    ,p.code AS "CODE"
    ,p.libelle AS "LIBELLE"
    ,SUBSTR(section.identifiant,8) AS "SECTION"
    ,SUBSTR(p.code,1,2) AS "TYPE"
    ,NULL AS "DOCUMENT"
    ,SUBSTR(cf.localite,9) AS "LOCALITE"
FROM
    poste p
LEFT JOIN SERVICE_CF cf ON cf.identifiant = p.domaine
LEFT JOIN VM_APP_SECTION section ON section.code = '327'
LEFT JOIN FONCTION fonction ON fonction.identifiant = p.fonction
WHERE
    fonction.code = 'CF' OR fonction.code = 'ACF';

CREATE OR REPLACE VIEW V_FONCTIONS_CPT
AS SELECT
    p.identifiant AS "IDENTIFIANT"
    ,p.code AS "CODE"
    ,p.libelle AS "LIBELLE"
    ,SUBSTR(section.identifiant,8) AS "SECTION"
    ,SUBSTR(p.code,1,2) AS "TYPE"
    ,NULL AS "DOCUMENT"
    ,SUBSTR(cpt.localite,9) AS "LOCALITE"
FROM
    poste p
LEFT JOIN SERVICE_CPT cpt ON cpt.identifiant = p.domaine
LEFT JOIN VM_APP_SECTION section ON section.code = '322'
LEFT JOIN FONCTION fonction ON fonction.identifiant = p.fonction
WHERE
    fonction.code = 'CPT' OR fonction.code = 'ACPT';

CREATE OR REPLACE VIEW V_FONCTIONS_EXECUTION
AS SELECT "IDENTIFIANT","CODE","LIBELLE","SECTION","TYPE","DOCUMENT","LOCALITE","CHAMP_ACTION" FROM V_FONCTIONS_GC gc
UNION ALL SELECT "IDENTIFIANT","CODE","LIBELLE","SECTION","TYPE","DOCUMENT","LOCALITE",NULL AS "CHAMP_ACTION" FROM V_FONCTIONS_ORD ord
UNION ALL SELECT "IDENTIFIANT","CODE","LIBELLE","SECTION","TYPE","DOCUMENT","LOCALITE",NULL AS "CHAMP_ACTION" FROM V_FONCTIONS_CF cf
UNION ALL SELECT "IDENTIFIANT","CODE","LIBELLE","SECTION","TYPE","DOCUMENT","LOCALITE",NULL AS "CHAMP_ACTION" FROM V_FONCTIONS_CPT cpt;

CREATE OR REPLACE VIEW V_ETAT_DM_POSTES_BUDGETAIRES
AS SELECT
    d.boite_postale AS "adresses_postale"
    ,c.libelle AS "civilite"
    ,d.code AS "code"
    ,TO_CHAR(d.date_signature_acte_nomination,'DD/MM/YYYY') AS "date_signature_acte_nomination"
    ,d.email AS "email"
    ,'2021' AS "exercice_budgetaire"
    ,'FB' AS "fonctions_budgetaires"
    ,d.fonction_administrative AS "fonction_administrative"
    ,d.identifiant AS "identifiant"
    ,d.matricule AS "matricule"
    ,d.nom||' '||d.prenoms AS "nom_prenoms"
    ,d.numero_mobile AS "numero_mobile"
    ,d.numero_bureau AS "numero_bureau"
    ,d.poste_bureau AS "poste_bureau"
    ,d.reference_acte_nomination AS "reference_acte_nomination"
    ,s.secb_code||' '||s.secb_libelle AS "section"
    ,d.signataire_acte_nomination AS "signataire_acte_nomination"
    ,'Fiche d''identification' AS "titre"
    ,g.libelle AS "type_utilisateur"
    ,ua.ua_code||' '||ua.ua_liblg AS "unite_administrative"
    ,d.photo AS "photo"
FROM
    DM_DEMANDE d
LEFT JOIN civilite c ON c.identifiant = d.civilite
LEFT JOIN groupe_identite g ON g.identifiant = d.groupe
LEFT JOIN SIIBC_CA.unite_administrative ua ON 'UA'||ua.uuid = d.unite_administrative
LEFT JOIN SIIBC_CA.section_budgetaire s ON s.uuid = ua.ua_secb_id
ORDER BY
    d.identifiant ASC;

CREATE OR REPLACE VIEW V_ETAT_DM_SPECIMEN_SIGNATURE
AS SELECT
    d.identifiant AS "identifiant"
    ,p.identifiant AS "poste"
    ,d.boite_postale AS "adresses_postale"
    ,c.libelle AS "civilite"
    ,d.code AS "code"
    ,TO_CHAR(d.date_signature_acte_nomination,'DD/MM/YYYY') AS "date_signature_acte_nomination"
    ,d.email AS "email"
    ,'2021' AS "exercice_budgetaire"
    ,p.code AS "code_fonction_budgetaire"
    ,p.libelle AS "libelle_fonction_budgetaire"
    ,p.code||' '||p.libelle AS "codelibelle_fb"
    ,d.fonction_administrative AS "fonction_administrative"
    ,d.matricule AS "matricule"
    ,d.nom||' '||d.prenoms AS "nom_prenoms"
    ,d.numero_mobile AS "numero_mobile"
    ,d.numero_bureau AS "numero_bureau"
    ,d.poste_bureau AS "poste_bureau"
    ,d.reference_acte_nomination AS "reference_acte_nomination"
    ,s.secb_code||' '||s.secb_libelle AS "section"
    ,d.signataire_acte_nomination AS "signataire_acte_nomination"
    ,UPPER('FICHE DE RENSEIGNEMENT ET DE DEP�T DE SIGNATURE DES '||(CASE SUBSTR(p.code,1,1) WHEN 'G' THEN 'GESTIONNAIRES DE CR�DITS' WHEN 'O' THEN 'ORDONNATEURS' ELSE 'XXX' END)) AS "titre"
    ,g.libelle AS "type_utilisateur"
    ,ua.ua_code||' '||ua.ua_liblg AS "unite_administrative"
    ,usb.usb_code||' '||usb.usb_liblg AS "USB"
    ,d.photo AS "photo"
FROM
    SIIBC_ACTOR.DM_POSTE dp
LEFT JOIN SIIBC_ACTOR.DM_DEMANDE d ON d.identifiant = dp.demande
LEFT JOIN SIIBC_ACTOR.CIVILITE c ON c.identifiant = d.civilite
LEFT JOIN SIIBC_ACTOR.GROUPE_IDENTITE g ON g.identifiant = d.groupe
LEFT JOIN SIIBC_ACTOR.POSTE p ON p.identifiant = dp.poste
LEFT JOIN SIIBC_CA.UNITE_ADMINISTRATIVE ua ON 'UA'||ua.uuid = d.unite_administrative
LEFT JOIN SIIBC_CA.SECTION_BUDGETAIRE s ON s.uuid = ua.ua_secb_id
LEFT JOIN SIIBC_ACTOR.service_ord ord ON ord.identifiant = p.domaine
LEFT JOIN SIIBC_CPP.USB usb ON 'USB'||usb.uuid = ord.usb
WHERE
    SUBSTR(p.code,1,1) IN ('G','O')
ORDER BY
    d.identifiant,dp.identifiant ASC;

-- Create MV

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
    vsql := 'MERGE INTO SIIBC_ACTOR.poste p USING SIIBC_MEA.fonction_execution f ON (p.identifiant = f.fonc_id) '
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
    vsql := 'MERGE INTO SIIBC_ACTOR.poste p USING SIIBC_MEA.fonction_execution f ON (p.identifiant = f.fonc_id) '
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
    vsql := 'MERGE INTO SIIBC_ACTOR.poste p USING SIIBC_MEA.fonction_execution f ON (p.identifiant = f.fonc_id) '
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
    vsql := 'MERGE INTO SIIBC_ACTOR.poste p USING SIIBC_MEA.fonction_execution f ON (p.identifiant = f.fonc_id) '
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
    USING SIIBC_ACTOR.affectations a 
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