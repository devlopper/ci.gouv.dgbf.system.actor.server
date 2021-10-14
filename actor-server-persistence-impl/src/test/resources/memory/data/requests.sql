INSERT INTO TYPE_DOMAINE (IDENTIFIANT,CODE,LIBELLE) VALUES ('SECTION','SECTION','Section');
INSERT INTO TYPE_DOMAINE (IDENTIFIANT,CODE,LIBELLE) VALUES ('UA','UA','Unité administrative');
INSERT INTO TYPE_DOMAINE (IDENTIFIANT,CODE,LIBELLE) VALUES ('USB','USB','USB');

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

INSERT INTO POSTE(IDENTIFIANT,CODE,LIBELLE,DOMAINE,FONCTION,NUMERO_DOCUMENT) VALUES ('GDTI','GDTI','GC DTI','DTI','GC',1)
INSERT INTO POSTE(IDENTIFIANT,CODE,LIBELLE,DOMAINE,FONCTION,NUMERO_DOCUMENT) VALUES ('AGDTI','AGDTI','AGC DTI','DTI','AGC',2)
INSERT INTO POSTE(IDENTIFIANT,CODE,LIBELLE,DOMAINE,FONCTION,NUMERO_DOCUMENT) VALUES ('OBUDGET','OBUDGET','ORD BUDGET','BUDGET','ORD',3)

INSERT INTO DM_TYPE(IDENTIFIANT,CODE,LIBELLE) VALUES ('DPB','DPB','Demande de poste budgétaire')
INSERT INTO DM_TYPE(IDENTIFIANT,CODE,LIBELLE) VALUES ('DPA','DPA','Demande de poste administrative')
INSERT INTO DM_STATUT(IDENTIFIANT,CODE,LIBELLE) VALUES ('I','INITIEE','Initié')
INSERT INTO DM_STATUT(IDENTIFIANT,CODE,LIBELLE) VALUES ('S','SOUMISE','Soumise')
INSERT INTO DM_STATUT(IDENTIFIANT,CODE,LIBELLE) VALUES ('A','ACCEPTEE','Accepté')
INSERT INTO DM_STATUT(IDENTIFIANT,CODE,LIBELLE) VALUES ('R','REJETEE','Rejeté')

INSERT INTO DM_BORDEREAU(IDENTIFIANT,CODE,LIBELLE,SECTION,FONCTION,DATE_CREATION) VALUES ('B001','B001','B001','327','GC', DATE '2000-1-1')
INSERT INTO DM_BORDEREAU(IDENTIFIANT,CODE,LIBELLE,SECTION,FONCTION,DATE_CREATION,DATE_ENVOI) VALUES ('B002','B002','B002','327','GC', DATE '2000-1-1', DATE '2000-1-2')
INSERT INTO DM_BORDEREAU(IDENTIFIANT,CODE,LIBELLE,SECTION,FONCTION,DATE_CREATION,DATE_ENVOI,DATE_TRAITEMENT) VALUES ('B003','B003','B003','327','GC', DATE '2000-1-1', DATE '2000-1-2', DATE '2000-1-3')

INSERT INTO DM_DEMANDE(IDENTIFIANT,TYPE,STATUT,CODE,EMAIL,NOM,PRENOMS,MATRICULE,UNITE_ADMINISTRATIVE,DATE_CREATION,DATE_TRAITEMENT,BORDEREAU) VALUES ('1','DPB','A','1','kycdev@gmail.com','Komenan','Yao Christian','498721Y','DTI',DATE '2000-1-1',DATE '2000-1-2','B001')
INSERT INTO DM_POSTE(IDENTIFIANT,DEMANDE,POSTE,ACCORDEE) VALUES ('1','1','GDTI',0)
INSERT INTO DM_POSTE(IDENTIFIANT,DEMANDE,POSTE,ACCORDEE) VALUES ('2','1','GDTI',1)
INSERT INTO DM_POSTE(IDENTIFIANT,DEMANDE,POSTE) VALUES ('3','1','AGDTI')
INSERT INTO DM_DEMANDE(IDENTIFIANT,TYPE,STATUT,CODE,EMAIL,NOM,PRENOMS,MATRICULE,UNITE_ADMINISTRATIVE,DATE_CREATION) VALUES ('2','DPA','I','2','kycdev@gmail.com','Komenan','Yao Christian','498721Y','DTI',DATE '2000-1-2')
INSERT INTO DM_POSTE(IDENTIFIANT,DEMANDE,POSTE) VALUES ('4','2','AGDTI')
INSERT INTO DM_DEMANDE(IDENTIFIANT,TYPE,STATUT,CODE,EMAIL,NOM,PRENOMS,MATRICULE,UNITE_ADMINISTRATIVE,DATE_CREATION) VALUES ('3','DPB','I','3','test@mail.com','Zadi','Gérard','100100A','DGDDL',DATE '2000-1-1')
INSERT INTO DM_POSTE(IDENTIFIANT,DEMANDE,POSTE) VALUES ('5','3','GDTI')
INSERT INTO DM_POSTE(IDENTIFIANT,DEMANDE,POSTE) VALUES ('6','3','OBUDGET')