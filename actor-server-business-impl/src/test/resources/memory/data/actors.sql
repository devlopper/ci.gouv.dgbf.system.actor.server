Insert into TYPE_DOMAINE (IDENTIFIANT,CODE,LIBELLE) values ('SECTION','SECTION','Section');
Insert into TYPE_PROFILE (IDENTIFIANT,CODE,LIBELLE) values ('SYSTEME','SYSTEME','Système');

Insert into VM_APP_DOMAINE (IDENTIFIANT,CODE,LIBELLE,TYPE) values ('s01','s01','Section 01','SECTION');
Insert into VM_APP_DOMAINE (IDENTIFIANT,CODE,LIBELLE,TYPE) values ('s02','s02','Section 02','SECTION');
Insert into VM_APP_DOMAINE (IDENTIFIANT,CODE,LIBELLE,TYPE) values ('s03','s03','Section 03','SECTION');
Insert into VM_APP_DOMAINE (IDENTIFIANT,CODE,LIBELLE,TYPE) values ('s04','s04','Section 04','SECTION');

Insert into PROFILE (IDENTIFIANT,CODE,LIBELLE,TYPE) values ('p01','p01','Profile 01','SYSTEME');
Insert into PROFILE (IDENTIFIANT,CODE,LIBELLE,TYPE) values ('p02','p02','Profile 02','SYSTEME');
Insert into PROFILE (IDENTIFIANT,CODE,LIBELLE,TYPE) values ('p03','p03','Profile 03','SYSTEME');
Insert into PROFILE (IDENTIFIANT,CODE,LIBELLE,TYPE) values ('p04','p04','Profile 04','SYSTEME');

Insert into ID_IDENTITE (IDENTIFIANT,NOM,PRENOMS,EMAIL) values ('1','komenan','christian','kycdev@gmail.com');
Insert into ACTEUR (IDENTIFIANT,NOM_UTILISATEUR,IDENTITE) values ('1','christian','1');
Insert into ID_IDENTITE (IDENTIFIANT,NOM,PRENOMS,EMAIL) values ('2','zadi','gérard','zd@gmail.com');
Insert into ACTEUR (IDENTIFIANT,NOM_UTILISATEUR,IDENTITE) values ('2','zd','2');

Insert into TA_DEMANDE_DOMAINE (IDENTIFIANT,ACTEUR,DOMAINE) values ('1','1','s02');
Insert into TA_DEMANDE_DOMAINE (IDENTIFIANT,ACTEUR,DOMAINE,ACCORDEE) values ('2','1','s03',true);
Insert into TA_DEMANDE_DOMAINE (IDENTIFIANT,ACTEUR,DOMAINE,ACCORDEE) values ('3','1','s04',false);

Insert into TA_DEMANDE_PROFILE (IDENTIFIANT,ACTEUR,PROFILE) values ('1','1','p02');
Insert into TA_DEMANDE_PROFILE (IDENTIFIANT,ACTEUR,PROFILE,ACCORDEE) values ('2','1','p03',true);
Insert into TA_DEMANDE_PROFILE (IDENTIFIANT,ACTEUR,PROFILE,ACCORDEE) values ('3','1','p04',false);