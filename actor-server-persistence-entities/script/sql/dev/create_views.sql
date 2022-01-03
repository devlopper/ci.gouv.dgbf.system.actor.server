CREATE OR REPLACE VIEW VA_UNITE_ADMINISTRATIVE AS
SELECT 'UA'||ua.uuid AS "IDENTIFIANT"
-- Sous-préfecture    
    ,CASE WHEN l.uuid IS NULL OR l.entitystatus <> 'COMMITTED' THEN NULL ELSE 'LOCALITE'||l.uuid END AS "SOUS_PREFECTURE_IDENTIFIANT"
    ,CASE WHEN l.uuid IS NULL OR l.entitystatus <> 'COMMITTED' THEN NULL ELSE l.loc_code||' '||l.loc_lib END AS "SOUS_PREFECTURE_CODE_LIBELLE"
-- Département
    ,(CASE WHEN LENGTH(l.loc_code) = 6 THEN 'LOCALITE'||(SELECT p.uuid FROM SIIBC_CA.LOCALITE p WHERE p.loc_code = SUBSTR(l.loc_code,1,LENGTH(l.loc_code)-2))
    ELSE NULL END) AS "DEPARTEMENT_IDENTIFIANT"
    ,(CASE WHEN LENGTH(l.loc_code) = 6 THEN (SELECT p.loc_code||' '||p.loc_lib FROM SIIBC_CA.LOCALITE p WHERE p.loc_code = SUBSTR(l.loc_code,1,LENGTH(l.loc_code)-2))
    ELSE NULL END) AS "DEPARTEMENT_CODE_LIBELLE"
-- Région
    ,(CASE WHEN LENGTH(l.loc_code) = 4 THEN 'LOCALITE'||(SELECT p.uuid FROM SIIBC_CA.LOCALITE p WHERE p.loc_code = SUBSTR(l.loc_code,1,LENGTH(l.loc_code)-2))
    WHEN LENGTH(l.loc_code) = 6 THEN 'LOCALITE'||(SELECT p.uuid FROM SIIBC_CA.LOCALITE p WHERE p.loc_code = SUBSTR(l.loc_code,1,LENGTH(l.loc_code)-4))
    ELSE NULL END) AS "REGION_IDENTIFIANT"
    ,(CASE WHEN LENGTH(l.loc_code) = 4 THEN (SELECT p.loc_code||' '||p.loc_lib FROM SIIBC_CA.LOCALITE p WHERE p.loc_code = SUBSTR(l.loc_code,1,LENGTH(l.loc_code)-2))
    WHEN LENGTH(l.loc_code) = 6 THEN (SELECT p.loc_code||' '||p.loc_lib FROM SIIBC_CA.LOCALITE p WHERE p.loc_code = SUBSTR(l.loc_code,1,LENGTH(l.loc_code)-4))
    ELSE NULL END) AS "REGION_CODE_LIBELLE"
FROM SIIBC_CA.unite_administrative ua,SIIBC_CA.section_budgetaire s,SIIBC_CA.localite l
WHERE s.uuid (+) = ua.ua_secb_id AND l.uuid (+) = ua.ua_loc_id;

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
    SIIBC_ACTEUR.DM_POSTE dp
LEFT JOIN SIIBC_ACTEUR.DM_DEMANDE d ON d.identifiant = dp.demande
LEFT JOIN SIIBC_ACTEUR.CIVILITE c ON c.identifiant = d.civilite
LEFT JOIN SIIBC_ACTEUR.GROUPE_IDENTITE g ON g.identifiant = d.groupe
LEFT JOIN SIIBC_ACTEUR.POSTE p ON p.identifiant = dp.poste
LEFT JOIN SIIBC_CA.UNITE_ADMINISTRATIVE ua ON 'UA'||ua.uuid = d.unite_administrative
LEFT JOIN SIIBC_CA.SECTION_BUDGETAIRE s ON s.uuid = ua.ua_secb_id
LEFT JOIN SIIBC_ACTEUR.service_ord ord ON ord.identifiant = p.domaine
LEFT JOIN SIIBC_CPP.USB usb ON 'USB'||usb.uuid = ord.usb
WHERE
    SUBSTR(p.code,1,1) IN ('G','O')
ORDER BY
    d.identifiant,dp.identifiant ASC;

-- Liste des postes et login
CREATE OR REPLACE VIEW VA_POSTE_LOGIN AS
SELECT DISTINCT fu.fonction_code||fu.login AS "IDENTIFIANT",fu.fonction_code AS "POSTE_CODE",fu.login AS "LOGIN"
FROM fonction_et_utilisateur@dblink_elabo_bidf fu
ORDER BY fu.login||fu.fonction_code ASC;

CREATE OR REPLACE VIEW VA_POSTE_LOGIN_GC_ORD AS
SELECT p.identifiant AS "POSTE",pl.login AS "LOGIN"
FROM VA_POSTE_LOGIN pl
JOIN Poste p ON p.code = pl.poste_code
WHERE pl.poste_code LIKE 'G%' OR pl.poste_code LIKE 'O%'
ORDER BY p.code ASC,pl.login ASC;

CREATE OR REPLACE VIEW VA_DM_POSTE_EMAIL_GC_ORD AS
SELECT p.identifiant AS "POSTE",d.email AS "EMAIL"
FROM DM_POSTE dp
JOIN DM_DEMANDE d ON d.identifiant = dp.demande AND d.statut = 'ACCEPTEE'
JOIN POSTE p ON p.identifiant = dp.poste AND (p.code LIKE 'G%' OR p.code LIKE 'O%')
WHERE dp.accordee = 1
ORDER BY p.code ASC,d.email ASC;

-- Liste des lignes importables
CREATE OR REPLACE VIEW VA_LIGNE_IMPORTABLE AS
SELECT l.* FROM vm_app_ex_imputation l WHERE l.exercice = 2021 AND l.ldep_id NOT IN (SELECT a.identifiant FROM affectations a)
UNION
SELECT l.* FROM vm_app_ex_imputation l WHERE l.exercice = 2022 AND l.identifiant NOT IN (SELECT a.identifiant FROM affectations a);

-- Liste des lignes exportables
CREATE OR REPLACE VIEW VA_LIGNE_EXPORTABLE AS
SELECT a.*,i.exercice,i.ldep_id
FROM affectations a
LEFT JOIN vm_app_ex_imputation i ON i.identifiant = a.imputation
WHERE a.etat = 'MODI';