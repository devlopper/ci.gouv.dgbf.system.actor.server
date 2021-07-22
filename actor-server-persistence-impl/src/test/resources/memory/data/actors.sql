Insert into VM_APP_SECTION (IDENTIFIANT,CODE,LIBELLE) values ('1','327','Budget');
Insert into VM_APP_UNITE_ADMINISTRATIVE (IDENTIFIANT,CODE,LIBELLE,SECTION) values ('DTI','DTI','Direction des traitements informatiques','1');

INSERT INTO GROUPE_IDENTITE (IDENTIFIANT,CODE,LIBELLE) VALUES ('F','F','Fonctionnaire');
INSERT INTO CIVILITE (IDENTIFIANT,CODE,LIBELLE) VALUES ('MR','MR','Monsieur');
INSERT INTO ID_IDENTITE (IDENTIFIANT,NOM,PRENOMS,EMAIL,CIVILITE,GROUPE,UNITE_ADMINISTRATIVE,FONCTION_ADMINISTRATIVE) VALUES ('1','Komenan','Yao Christian','kycdev@gmail.com','MR','F','DTI','Chef de service');
INSERT INTO ID_IDENTITE (IDENTIFIANT,NOM,PRENOMS,EMAIL,CIVILITE) VALUES ('2','Admin','Administrateur','admin@mail.com','MR');
INSERT INTO ID_IDENTITE (IDENTIFIANT,NOM,PRENOMS,EMAIL,CIVILITE,GROUPE,UNITE_ADMINISTRATIVE,FONCTION_ADMINISTRATIVE) VALUES ('3','Komenan','Yao Christian','kycdev@gmail02.com','MR','F','DTI','Chef de service');

INSERT INTO ACTEUR (IDENTIFIANT,NOM_UTILISATEUR,IDENTITE) VALUES ('1','christian','1');
INSERT INTO ACTEUR (IDENTIFIANT,NOM_UTILISATEUR,IDENTITE) VALUES ('2','admin','2');
INSERT INTO ACTEUR (IDENTIFIANT,NOM_UTILISATEUR,IDENTITE) VALUES ('3','christianos','3');