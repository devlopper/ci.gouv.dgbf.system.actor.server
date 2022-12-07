CREATE OR REPLACE VIEW VA_UNITE_ADMINISTRATIVE AS
SELECT 'UA'||ua.uuid AS "IDENTIFIANT"
-- Sous-préfecture    
    ,CASE WHEN l.uuid IS NULL OR l.entitystatus <> 'COMMITTED' THEN NULL ELSE 'LOCALITE'||l.uuid END AS "SOUS_PREFECTURE_IDENTIFIANT"
    ,CASE WHEN l.uuid IS NULL OR l.entitystatus <> 'COMMITTED' THEN NULL ELSE l.loc_code||' '||l.loc_lib END AS "SOUS_PREFECTURE_CODE_LIBELLE"
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
FROM CA.unite_administrative ua,CA.section_budgetaire s,CA.localite l
WHERE s.uuid (+) = ua.ua_secb_id AND l.uuid (+) = ua.ua_loc_id;

CREATE OR REPLACE VIEW V_FONCTIONS_GC
AS SELECT
    p.identifiant AS "IDENTIFIANT"
    ,p.code AS "CODE"
    ,p.libelle AS "LIBELLE"
    ,SUBSTR(section.identifiant,8) AS "SECTION"
    ,section.code||' '||section.libelle AS "SECTION_CODE_LIBELLE"
    ,SUBSTR(p.code,1,2) AS "TYPE"
    ,SUBSTR(localite.identifiant,9) AS "LOCALITE"
    ,p.numero_document AS "NUMERO_DOCUMENT"
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
    ,section.code||' '||section.libelle AS "SECTION_CODE_LIBELLE"
    ,SUBSTR(p.code,1,2) AS "TYPE"
    ,SUBSTR(ord.localite,9) AS "LOCALITE"
    ,p.numero_document AS "NUMERO_DOCUMENT"
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
    ,SUBSTR(cf.localite,9) AS "LOCALITE"
    ,p.numero_document AS "NUMERO_DOCUMENT"
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
    ,SUBSTR(cpt.localite,9) AS "LOCALITE"
    ,p.numero_document AS "NUMERO_DOCUMENT"
FROM
    poste p
LEFT JOIN SERVICE_CPT cpt ON cpt.identifiant = p.domaine
LEFT JOIN VM_APP_SECTION section ON section.code = '322'
LEFT JOIN FONCTION fonction ON fonction.identifiant = p.fonction
WHERE
    fonction.code = 'CPT' OR fonction.code = 'ACPT';

CREATE OR REPLACE VIEW V_FONCTIONS_EXECUTION
AS SELECT "IDENTIFIANT","CODE","LIBELLE","SECTION","TYPE","NUMERO_DOCUMENT","LOCALITE","CHAMP_ACTION" FROM V_FONCTIONS_GC gc
UNION ALL SELECT "IDENTIFIANT","CODE","LIBELLE","SECTION","TYPE","NUMERO_DOCUMENT","LOCALITE",NULL AS "CHAMP_ACTION" FROM V_FONCTIONS_ORD ord
UNION ALL SELECT "IDENTIFIANT","CODE","LIBELLE","SECTION","TYPE","NUMERO_DOCUMENT","LOCALITE",NULL AS "CHAMP_ACTION" FROM V_FONCTIONS_CF cf
UNION ALL SELECT "IDENTIFIANT","CODE","LIBELLE","SECTION","TYPE","NUMERO_DOCUMENT","LOCALITE",NULL AS "CHAMP_ACTION" FROM V_FONCTIONS_CPT cpt;

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
LEFT JOIN CA.unite_administrative ua ON 'UA'||ua.uuid = d.unite_administrative
LEFT JOIN CA.section_budgetaire s ON s.uuid = ua.ua_secb_id
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
    --,usb.usb_code||' '||usb.usb_liblg AS "USB"
    ,CASE WHEN ord.usb IS NULL THEN usb_domaine.usb_code||' '||usb_domaine.usb_liblg ELSE usb.usb_code||' '||usb.usb_liblg END AS "USB"
    ,d.photo AS "photo"
FROM
    DM_POSTE dp
LEFT JOIN ACTEUR.DM_DEMANDE d ON d.identifiant = dp.demande
LEFT JOIN ACTEUR.CIVILITE c ON c.identifiant = d.civilite
LEFT JOIN ACTEUR.GROUPE_IDENTITE g ON g.identifiant = d.groupe
LEFT JOIN ACTEUR.POSTE p ON p.identifiant = dp.poste
LEFT JOIN CA.UNITE_ADMINISTRATIVE ua ON 'UA'||ua.uuid = d.unite_administrative
LEFT JOIN CA.SECTION_BUDGETAIRE s ON s.uuid = ua.ua_secb_id
LEFT JOIN ACTEUR.service_ord ord ON ord.identifiant = p.domaine
LEFT JOIN CPP.USB usb ON 'USB'||usb.uuid = ord.usb
LEFT JOIN CPP.USB usb_domaine ON 'USB'||usb_domaine.uuid = p.domaine
WHERE
    SUBSTR(p.code,1,1) IN ('G','O')
ORDER BY
    d.identifiant,dp.identifiant ASC;

CREATE OR REPLACE VIEW V_ETAT_DM_BORDEREAUX
AS SELECT
    b.code AS "bordereau"
    ,'Bordereau de demandes de '||f.libelle AS "titre"
    ,section.secb_code||' '||section.secb_libelle AS "section"
    ,d.boite_postale AS "boite_postale"
    ,c.libelle AS "civilite"
    ,d.code AS "code"
    ,d.email AS "email"
    ,d.fonction_administrative AS "fonction_administrative"
    ,d.matricule AS "matricule"
    ,d.nom AS "nom"
    ,d.numero_bureau AS "numero_bureau"
    ,d.numero_mobile AS "numero_mobile"
    ,d.poste_bureau AS "poste_bureau"
    ,d.prenoms AS "prenoms"
--    ,section.secb_code||' '||section.secb_libelle AS "section"
    ,ua.ua_code||' '||ua.ua_liblg AS "unite_administrative"
    ,p.libelle AS "fonction"
FROM
    DM_DEMANDE d
LEFT JOIN dm_bordereau b ON b.identifiant = d.bordereau
LEFT JOIN civilite c ON c.identifiant = d.civilite
LEFT JOIN fonction f ON f.identifiant = b.fonction
LEFT JOIN dm_poste pd ON pd.demande = d.identifiant
LEFT JOIN poste p ON p.identifiant = pd.poste
LEFT JOIN CA.section_budgetaire section ON section.uuid = SUBSTR(d.section,8)
LEFT JOIN CA.unite_administrative ua ON ua.uuid = SUBSTR(d.unite_administrative,3)
--LEFT JOIN CA.section_budgetaire section ON section.uuid = ua.ua_secb_id
WHERE b.identifiant IS NOT NULL --AND b.fonction = dp.
ORDER BY d.code ASC;

CREATE OR REPLACE VIEW V_ASSISTANTS_NON_RATTACHES
AS SELECT a.identifiant,a.code,a.libelle,a.parent FROM poste a WHERE a.code LIKE 'A%' AND a.parent IS NULL;

-- Liste des lignes importables
--CREATE OR REPLACE VIEW VA_LIGNE_IMPORTABLE AS
--SELECT l.* FROM vm_app_ex_imputation l WHERE l.exercice = 2021 AND l.ldep_id NOT IN (SELECT a.identifiant FROM affectations a)
--UNION
--SELECT l.* FROM vm_app_ex_imputation l WHERE l.exercice = 2022 AND l.identifiant NOT IN (SELECT a.identifiant FROM affectations a);

CREATE OR REPLACE VIEW VA_LIGNE_IMPORTABLE AS
SELECT l.*,CASE WHEN ld.etat = 'PRCH' THEN l.etat ELSE ld.etat END AS "ETAT_BIDF"
FROM vm_app_ex_imputation l LEFT JOIN ligne_de_depenses@dblink_elabo_bidf ld ON ld.ldep_id = l.ldep_id AND ld.exo_num = l.exercice
WHERE l.exercice = 2021 AND l.ldep_id NOT IN (SELECT a.identifiant FROM affectations a)
UNION
SELECT l.*,CASE WHEN ld.etat = 'PRCH' THEN l.etat ELSE ld.etat END AS "ETAT_BIDF"
FROM vm_app_ex_imputation l LEFT JOIN ligne_de_depenses@dblink_elabo_bidf ld ON ld.ldep_id = l.ldep_id AND ld.exo_num = l.exercice
WHERE l.exercice = 2022 AND l.identifiant NOT IN (SELECT a.identifiant FROM affectations a);

-- Liste des lignes exportables
CREATE OR REPLACE VIEW VA_LIGNE_EXPORTABLE AS
SELECT a.*,i.exercice,i.ldep_id
FROM affectations a
LEFT JOIN vm_app_ex_imputation i ON i.identifiant = a.imputation
WHERE a.etat = 'MODI';

-------------------------------------------------------------------------------------------------------

CREATE OR REPLACE VIEW VA_POSTE_LIBERE_EN_EXECUTION AS
SELECT fonction_code AS "CODE",CAST('SYSTEME_EXECUTION'AS VARCHAR2(255)) AS "LIBERE_PAR",MAX(date_fin) AS "LIBERE_LE"
FROM fonction_et_utilisateur@dblink_elabo_bidf
WHERE fonction_code NOT LIKE 'A%' AND fonction_code NOT LIKE 'B%' AND date_fin is not null
GROUP BY fonction_code
ORDER BY MAX(date_fin) DESC;

CREATE OR REPLACE VIEW VA_POSTE_LIBRE_EXECUTION AS
SELECT p."IDENTIFIANT",p."CODE",p."LIBELLE",p."NOMBRE_ACTEUR",p."FONCTION",p."DOMAINE",p."PARENT",p."LOCALITE",p."ACTIVITE",p."NUMERO_DOCUMENT",p."DATE_CODIFICATION",p."NUMERO_ORDRE",p."AUDIT_ACTEUR",p."AUDIT_ACTION",p."AUDIT_DATE",p."AUDIT_FONCTIONALITE",p."ETAT",p."DATE_ETAT",p."CODE_FONCTION_SIB",p."NOMS_UTILISATEURS"
FROM POSTE p
WHERE p.code NOT IN (SELECT poste FROM v_app_ex_poste_utilisateur);

CREATE OR REPLACE VIEW VA_POSTE_ACCORDE_DEMANDE AS
SELECT p."IDENTIFIANT",p."CODE",p."LIBELLE",p."NOMBRE_ACTEUR",p."FONCTION",p."DOMAINE",p."PARENT",p."LOCALITE",p."ACTIVITE",p."NUMERO_DOCUMENT",p."DATE_CODIFICATION",p."NUMERO_ORDRE",p."AUDIT_ACTEUR",p."AUDIT_ACTION",p."AUDIT_DATE",p."AUDIT_FONCTIONALITE",p."ETAT",p."DATE_ETAT",p."CODE_FONCTION_SIB",p."NOMS_UTILISATEURS"
FROM POSTE p
WHERE p.identifiant IN (
    SELECT dp.poste
    FROM dm_poste dp
    WHERE dp.accordee = 1
    );

CREATE OR REPLACE VIEW VA_POST_A_LIBERE_POUR_DEMAND AS
SELECT p."IDENTIFIANT",p."CODE",p."LIBELLE",p."NOMBRE_ACTEUR",p."FONCTION",p."DOMAINE",p."PARENT",p."LOCALITE",p."ACTIVITE",p."NUMERO_DOCUMENT",p."DATE_CODIFICATION",p."NUMERO_ORDRE",p."AUDIT_ACTEUR",p."AUDIT_ACTION",p."AUDIT_DATE",p."AUDIT_FONCTIONALITE",p."ETAT",p."DATE_ETAT",p."CODE_FONCTION_SIB",p."NOMS_UTILISATEURS",ple.libere_par AS "LIBERE_PAR"
FROM POSTE p
JOIN VA_POSTE_LIBERE_EN_EXECUTION ple ON ple.code = p.code
WHERE p.code NOT LIKE 'A%' AND p.identifiant IN (SELECT identifiant FROM VA_POSTE_LIBRE_EXECUTION) AND p.identifiant IN (SELECT identifiant FROM VA_POSTE_ACCORDE_DEMANDE)
ORDER BY p.code ASC;