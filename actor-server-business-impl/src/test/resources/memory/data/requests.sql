INSERT INTO TYPE_DOMAINE (IDENTIFIANT,CODE,LIBELLE) VALUES ('SECTION','SECTION','Section');
INSERT INTO TYPE_DOMAINE (IDENTIFIANT,CODE,LIBELLE) VALUES ('UA','UA','Unité administrative');
INSERT INTO TYPE_DOMAINE (IDENTIFIANT,CODE,LIBELLE) VALUES ('USB','USB','USB');
Insert into TYPE_DOMAINE (IDENTIFIANT,CODE,LIBELLE,NUMERO_ORDRE) values ('CATEGORIE_BUDGET','CATEGORIE_BUDGET','Catégorie Budget',3);

INSERT INTO VM_APP_CAT_BUDGET (IDENTIFIANT,CODE,LIBELLE) VALUES ('CB01','01','Budget G');
INSERT INTO VM_APP_DOMAINE (IDENTIFIANT,CODE,LIBELLE,TYPE) VALUES ('CB01','01','Budget G','CATEGORIE_BUDGET');

INSERT INTO VM_APP_SECTION (IDENTIFIANT,CODE,LIBELLE) VALUES ('335','335','Sante');
INSERT INTO VM_APP_DOMAINE (IDENTIFIANT,CODE,LIBELLE,TYPE) VALUES ('335','335','Sante','SECTION');
INSERT INTO VM_APP_SECTION (IDENTIFIANT,CODE,LIBELLE) VALUES ('327','327','Budget');
INSERT INTO VM_APP_DOMAINE (IDENTIFIANT,CODE,LIBELLE,TYPE) VALUES ('327','327','Budget','SECTION');
INSERT INTO VM_APP_SECTION (IDENTIFIANT,CODE,LIBELLE) VALUES ('323','323','Intérieur');
INSERT INTO VM_APP_DOMAINE (IDENTIFIANT,CODE,LIBELLE,TYPE) VALUES ('323','323','Intérieur','SECTION');
INSERT INTO VM_APP_SECTION (IDENTIFIANT,CODE,LIBELLE) VALUES ('s03','s03','Section 03');
INSERT INTO VM_APP_DOMAINE (IDENTIFIANT,CODE,LIBELLE,TYPE) VALUES ('s03','s03','Section 03','SECTION');

INSERT INTO TYPE_FONCTION (IDENTIFIANT,CODE,LIBELLE) VALUES ('BUDGETAIRE','BUDGETAIRE','BUDGETAIRE');
INSERT INTO FONCTION (IDENTIFIANT,CODE,LIBELLE) VALUES ('GC','GC','GC');
INSERT INTO FONCTION (IDENTIFIANT,CODE,LIBELLE) VALUES ('AGC','AGC','AGC');
INSERT INTO FONCTION (IDENTIFIANT,CODE,LIBELLE) VALUES ('ORD','ORD','ORD');

INSERT INTO VM_APP_DOMAINE (IDENTIFIANT,CODE,LIBELLE,TYPE) VALUES ('DSIB','DSIB','DSIB','UA');
INSERT INTO VM_APP_UNITE_ADMINISTRATIVE (IDENTIFIANT,CODE,LIBELLE,SECTION) VALUES ('DSIB','DSIB','DSIB','327');
INSERT INTO VM_APP_DOMAINE (IDENTIFIANT,CODE,LIBELLE,TYPE) VALUES ('CHUYOP','CHUYOP','CHUYOP','UA');
INSERT INTO VM_APP_UNITE_ADMINISTRATIVE (IDENTIFIANT,CODE,LIBELLE,SECTION) VALUES ('CHUYOP','CHUYOP','CHUYOP','335');

INSERT INTO VM_APP_UNITE_ADMINISTRATIVE (IDENTIFIANT,CODE,LIBELLE,SECTION) VALUES ('DBE','13010221','DBE','327');
INSERT INTO VM_APP_DOMAINE (IDENTIFIANT,CODE,LIBELLE,TYPE) VALUES ('DBE','13010221','DBE','UA');
INSERT INTO VM_APP_UNITE_ADMINISTRATIVE (IDENTIFIANT,CODE,LIBELLE,SECTION) VALUES ('DTI','13010222','DTI','327');
INSERT INTO VM_APP_DOMAINE (IDENTIFIANT,CODE,LIBELLE,TYPE) VALUES ('DTI','13010222','DTI','UA');
INSERT INTO VM_APP_UNITE_ADMINISTRATIVE (IDENTIFIANT,CODE,LIBELLE,SECTION) VALUES ('DGDDL','13010220','DGDDL','323');
INSERT INTO VM_APP_DOMAINE (IDENTIFIANT,CODE,LIBELLE,TYPE) VALUES ('DGDDL','13010220','DGDDL','UA');
INSERT INTO VM_APP_UNITE_ADMINISTRATIVE (IDENTIFIANT,CODE,LIBELLE,SECTION) VALUES ('ua03','ua03','ua 03','s03');
INSERT INTO VM_APP_DOMAINE (IDENTIFIANT,CODE,LIBELLE,TYPE) VALUES ('ua03','ua03','ua 03','UA');
INSERT INTO VM_APP_UNITE_ADMINISTRATIVE (IDENTIFIANT,CODE,LIBELLE,SECTION) VALUES ('BUDGET','BUDGET','BUDGET','s03');
INSERT INTO VM_APP_DOMAINE (IDENTIFIANT,CODE,LIBELLE,TYPE) VALUES ('BUDGET','BUDGET','BUDGET','USB');

INSERT INTO CATEGORIE_POSTE(IDENTIFIANT,CODE,LIBELLE,CATEGORIE_BUDGET) VALUES ('G6','G6','G6','6');

INSERT INTO POSTE(IDENTIFIANT,CODE,LIBELLE,DOMAINE,FONCTION,NUMERO_DOCUMENT) VALUES ('GDTI','GDTI','GC DTI','DTI','GC',1)
INSERT INTO POSTE(IDENTIFIANT,CODE,LIBELLE,DOMAINE,FONCTION,NUMERO_DOCUMENT) VALUES ('AGDTI','AGDTI','AGC DTI','DTI','AGC',2)
INSERT INTO POSTE(IDENTIFIANT,CODE,LIBELLE,DOMAINE,FONCTION,NUMERO_DOCUMENT) VALUES ('OBUDGET','OBUDGET','ORD BUDGET','BUDGET','ORD',3)
INSERT INTO POSTE(IDENTIFIANT,CODE,LIBELLE,FONCTION,DOMAINE,NUMERO_DOCUMENT) values ('GDSIB','GDSIB','GDSIB','GC','DSIB',4);
INSERT INTO POSTE(IDENTIFIANT,CODE,LIBELLE,FONCTION,DOMAINE,NUMERO_DOCUMENT,CATEGORIE) VALUES ('GCHUYOP','GCHUYOP','GCHUYOP','GC','CHUYOP',5,'G6');
INSERT INTO POSTE(IDENTIFIANT,CODE,LIBELLE,FONCTION,DOMAINE,NUMERO_DOCUMENT) values ('GDBE','GDBE','GDBE','GC','DBE',6);

INSERT INTO ID_FORMULAIRE(IDENTIFIANT,CODE,LIBELLE) VALUES ('1','1','1')
INSERT INTO DM_TYPE(IDENTIFIANT,CODE,LIBELLE,FORMULAIRE) VALUES ('DPB','DEMANDE_POSTES_BUDGETAIRES','Demande de poste budgétaire','1')
INSERT INTO DM_TYPE(IDENTIFIANT,CODE,LIBELLE) VALUES ('DPA','DPA','Demande de poste administrative')
INSERT INTO DM_STATUT(IDENTIFIANT,CODE,LIBELLE) VALUES ('I','INITIEE','Initié')
INSERT INTO DM_STATUT(IDENTIFIANT,CODE,LIBELLE) VALUES ('S','SOUMISE','Soumise')
INSERT INTO DM_STATUT(IDENTIFIANT,CODE,LIBELLE) VALUES ('A','ACCEPTEE','Accepté')
INSERT INTO DM_STATUT(IDENTIFIANT,CODE,LIBELLE) VALUES ('R','REJETEE','Rejeté')

INSERT INTO DM_BORDEREAU(IDENTIFIANT,CODE,LIBELLE,SECTION,FONCTION,DATE_CREATION,DATE_ENVOI) VALUES ('1','1','1','327','GC', DATE '2000-1-1', DATE '2000-1-2')

INSERT INTO DM_DEMANDE(IDENTIFIANT,TYPE,STATUT,CODE,EMAIL,DATE_CREATION,BORDEREAU) VALUES ('1','DPB','S','1','a@m.com', DATE '2000-1-1','1')
INSERT INTO DM_POSTE(IDENTIFIANT,DEMANDE,POSTE) VALUES ('1','1','GDTI')
