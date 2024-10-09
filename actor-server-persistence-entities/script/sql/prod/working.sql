-- Visibilité

CREATE TABLE "ACTEUR"."TT_CHRGMNT_VISIBILITE" 
   ("NOM_UTILISATEUR" VARCHAR2(1024 BYTE) NOT NULL ENABLE, 
	"CODE_TYPE_DOMAINE" VARCHAR2(10 BYTE) NOT NULL ENABLE, 
	"CODE_DOMAINE" VARCHAR2(30 BYTE) NOT NULL ENABLE, 
	"INVISIBLE" VARCHAR2(255 BYTE), 
	 CONSTRAINT "TT_CHRGMNT_VISIBILITE_PK" PRIMARY KEY ("NOM_UTILISATEUR", "CODE_TYPE_DOMAINE", "CODE_DOMAINE") ENABLE
   );

CREATE TABLE "ACTEUR"."TT_CHRGMNT_VISIBILITE_LOG" 
   ("NOM_UTILISATEUR" VARCHAR2(1024 BYTE) NOT NULL ENABLE, 
	"CODE_TYPE_DOMAINE" VARCHAR2(10 BYTE) NOT NULL ENABLE, 
	"CODE_DOMAINE" VARCHAR2(30 BYTE) NOT NULL ENABLE, 
	"INVISIBLE" VARCHAR2(255 BYTE),
	"ACTION" VARCHAR2(10 BYTE) NOT NULL ENABLE, 
	"IDENTIFIANT" VARCHAR2(40 BYTE) NOT NULL ENABLE, 
	"DATE_" DATE NOT NULL ENABLE
   );
   
CREATE OR REPLACE VIEW VT_CHRGMNT_VISIBILITE AS
SELECT acteur.identifiant AS "ACTEUR", domaine.identifiant AS "DOMAINE", CASE WHEN t.invisible IS NULL THEN 1 ELSE 0 END AS "VISIBLE"
, t.nom_utilisateur AS "NOM_UTILISATEUR", t.code_type_domaine AS "CODE_TYPE_DOMAINE", t.code_domaine AS "CODE_DOMAINE", t.invisible
FROM TT_CHRGMNT_VISIBILITE t
JOIN ACTEUR acteur ON acteur.nom_utilisateur = t.nom_utilisateur
JOIN TYPE_DOMAINE type_domaine ON type_domaine.code = t.code_type_domaine
JOIN VM_APP_DOMAINE domaine ON domaine.code = t.code_domaine AND domaine.type = type_domaine.identifiant;

CREATE OR REPLACE PROCEDURE PT_CHARGER_VISIBILITE AUTHID CURRENT_USER AS
ocount NUMBER(6);
date_ DATE := SYSDATE();
action VARCHAR2(32);
identifiant VARCHAR2(255);
BEGIN
    FOR t IN (SELECT * FROM VT_CHRGMNT_VISIBILITE)
    LOOP
        -- application
        SELECT COUNT(*) INTO ocount FROM acteur_domaine WHERE acteur = t.acteur AND domaine = t.domaine;
        IF ocount > 0 THEN
            action := 'UPDATE';
            SELECT e.identifiant INTO identifiant FROM acteur_domaine e WHERE e.acteur = t.acteur AND e.domaine = t.domaine;
            EXECUTE IMMEDIATE 'UPDATE ACTEUR_DOMAINE SET visible = :1 WHERE acteur=:2 AND domaine=:3' USING t.visible,t.acteur,t.domaine;
        ELSE
            action := 'INSERT';
            identifiant := SYS_GUID();
            EXECUTE IMMEDIATE 'INSERT INTO ACTEUR_DOMAINE(identifiant,acteur,domaine,visible) VALUES (:1,:2,:3,:4)' USING identifiant,t.acteur,t.domaine,t.visible;
        END IF;
        -- historisation
        EXECUTE IMMEDIATE 'INSERT INTO tt_chrgmnt_visibilite_log(nom_utilisateur,code_type_domaine,code_domaine,invisible,action,identifiant,date_) VALUES (:1,:2,:3,:4,:5,:6,:7)' USING t.nom_utilisateur,t.code_type_domaine,t.code_domaine,t.invisible,action,identifiant,date_;
        -- suppression
        EXECUTE IMMEDIATE 'DELETE FROM tt_chrgmnt_visibilite WHERE nom_utilisateur=:1 AND code_type_domaine=:2 AND code_domaine=:3' USING t.nom_utilisateur,t.code_type_domaine,t.code_domaine;
        COMMIT;
    END LOOP;
END;

-- Profile

CREATE TABLE "ACTEUR"."TT_CHRGMNT_PROFILE" 
   ("NOM_UTILISATEUR" VARCHAR2(1024 BYTE) NOT NULL ENABLE, 
	"CODE_PROFILE" VARCHAR2(20 BYTE) NOT NULL ENABLE,
	"INVISIBLE" VARCHAR2(255 BYTE),
	 CONSTRAINT "TT_CHRGMNT_PROFILE_PK" PRIMARY KEY ("NOM_UTILISATEUR", "CODE_PROFILE") ENABLE
   );

CREATE TABLE "ACTEUR"."TT_CHRGMNT_PROFILE_LOG" 
   ("NOM_UTILISATEUR" VARCHAR2(1024 BYTE) NOT NULL ENABLE, 
	"CODE_PROFILE" VARCHAR2(20 BYTE) NOT NULL ENABLE,  
	"INVISIBLE" VARCHAR2(255 BYTE),
	"ACTION" VARCHAR2(10 BYTE) NOT NULL ENABLE, 
	"IDENTIFIANT" VARCHAR2(40 BYTE) NOT NULL ENABLE, 
	"DATE_" DATE NOT NULL ENABLE
   );
   
CREATE OR REPLACE VIEW VT_CHRGMNT_PROFILE AS
SELECT acteur.identifiant AS "ACTEUR", profile.identifiant AS "PROFILE", CASE WHEN t.invisible IS NULL THEN 1 ELSE 0 END AS "VISIBLE"
, t.nom_utilisateur AS "NOM_UTILISATEUR", t.code_profile AS "CODE_PROFILE", t.invisible
FROM TT_CHRGMNT_PROFILE t
JOIN ACTEUR acteur ON acteur.nom_utilisateur = t.nom_utilisateur
JOIN PROFILE profile ON profile.code = t.code_profile;

CREATE OR REPLACE PROCEDURE PT_CHARGER_PROFILE AUTHID CURRENT_USER AS
ocount NUMBER(6);
date_ DATE := SYSDATE();
action VARCHAR2(32);
identifiant VARCHAR2(255);
BEGIN
    FOR t IN (SELECT * FROM VT_CHRGMNT_PROFILE)
    LOOP
        -- application
        SELECT COUNT(*) INTO ocount FROM acteur_profile WHERE acteur = t.acteur AND profile = t.profile;
        IF ocount > 0 THEN
            action := 'DELETE';
            SELECT e.identifiant INTO identifiant FROM acteur_profile e WHERE e.acteur = t.acteur AND e.profile = t.profile;
            EXECUTE IMMEDIATE 'DELETE ACTEUR_PROFILE WHERE identifiant = :1' USING identifiant;
        ELSE
            action := 'INSERT';
            identifiant := SYS_GUID();
            EXECUTE IMMEDIATE 'INSERT INTO ACTEUR_PROFILE(identifiant,acteur,profile) VALUES (:1,:2,:3)' USING identifiant,t.acteur,t.profile;
        END IF;
        -- historisation
        EXECUTE IMMEDIATE 'INSERT INTO tt_chrgmnt_profile_log(nom_utilisateur,code_profile,invisible,action,identifiant,date_) VALUES (:1,:2,:3,:4,:5,:6)' USING t.nom_utilisateur,t.code_profile,t.invisible,action,identifiant,date_;
        -- suppression
        EXECUTE IMMEDIATE 'DELETE FROM tt_chrgmnt_profile WHERE nom_utilisateur=:1 AND code_profile=:2' USING t.nom_utilisateur,t.code_profile;
        COMMIT;
    END LOOP;
END;