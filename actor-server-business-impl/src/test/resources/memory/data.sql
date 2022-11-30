--DROP ALIAS IF EXISTS P_EXPORTER_AFFECTATIONS;
--CREATE ALIAS P_EXPORTER_AFFECTATIONS FOR "ci.gouv.dgbf.system.actor.server.business.impl.unit.BusinessImplUnitTest.p_exporter_affectations";

Insert into ID_FORMULAIRE (IDENTIFIANT,CODE,LIBELLE) values ('1','1','Formulaire de demande de postes budgétaires');

Insert into DM_STATUT (IDENTIFIANT,CODE,LIBELLE) values ('INITIEE','INITIEE','Initiée');
Insert into DM_STATUT (IDENTIFIANT,CODE,LIBELLE) values ('ACCEPTEE','ACCEPTEE','Acceptée');
Insert into DM_STATUT (IDENTIFIANT,CODE,LIBELLE) values ('REJETEE','REJETEE','Rejetée');
Insert into DM_STATUT (IDENTIFIANT,CODE,LIBELLE) values ('SOUMISE','SOUMISE','Soumise');

Insert into DM_TYPE (IDENTIFIANT,CODE,LIBELLE,FORMULAIRE) values ('DPB','DEMANDE_POSTES_BUDGETAIRES','Demande de postes budgétaires','1');

Insert into TYPE_DOMAINE (IDENTIFIANT,CODE,LIBELLE,NUMERO_ORDRE) values ('SECTION','SECTION','Section',1);
Insert into TYPE_DOMAINE (IDENTIFIANT,CODE,LIBELLE,NUMERO_ORDRE) values ('USB','USB','Unité de spécialisation du budget',2);
Insert into TYPE_DOMAINE (IDENTIFIANT,CODE,LIBELLE,NUMERO_ORDRE) values ('CATEGORIE_ACTIVITE','CATEGORIE_ACTIVITE','Catégorie Activité',3);
Insert into TYPE_DOMAINE (IDENTIFIANT,CODE,LIBELLE,NUMERO_ORDRE) values ('CATEGORIE_BUDGET','CATEGORIE_BUDGET','Catégorie Budget',3);
Insert into TYPE_DOMAINE (IDENTIFIANT,CODE,LIBELLE,NUMERO_ORDRE) values ('ACTIVITE','ACTIVITE','Activité',4);
Insert into TYPE_DOMAINE (IDENTIFIANT,CODE,LIBELLE,NUMERO_ORDRE) values ('UA','UA','Unité administrative',5);
Insert into TYPE_DOMAINE (IDENTIFIANT,CODE,LIBELLE,NUMERO_ORDRE) values ('LOCALITE','LOCALITE','Localité',6);
Insert into TYPE_DOMAINE (IDENTIFIANT,CODE,LIBELLE,NUMERO_ORDRE) values ('SERVICE_ORD','SERVICE_ORD','Services ordonnateurs',7);
Insert into TYPE_DOMAINE (IDENTIFIANT,CODE,LIBELLE,NUMERO_ORDRE) values ('SERVICE_CF','SERVICE_CF','Services contrôleurs financiers',8);
Insert into TYPE_DOMAINE (IDENTIFIANT,CODE,LIBELLE,NUMERO_ORDRE) values ('SERVICE_CPT','SERVICE_CPT','Postes comptables',9);

INSERT INTO VM_APP_CAT_BUDGET (IDENTIFIANT,CODE,LIBELLE) VALUES ('CB01','01','Budget G');
INSERT INTO VM_APP_DOMAINE (IDENTIFIANT,CODE,LIBELLE,TYPE) VALUES ('CB01','01','Budget G','CATEGORIE_BUDGET');

Insert into VM_APP_DOMAINE (IDENTIFIANT,CODE,LIBELLE,TYPE) values ('13010222','13010222','DTI','UA');
Insert into VM_APP_DOMAINE (IDENTIFIANT,CODE,LIBELLE,TYPE) values ('13010223','13010223','DSI','UA');
Insert into VM_APP_DOMAINE (IDENTIFIANT,CODE,LIBELLE,TYPE) values ('22086','22086','Budget','USB');
Insert into VM_APP_DOMAINE (IDENTIFIANT,CODE,LIBELLE,TYPE) values ('ORD001','ORD001','Budget Dimbokro','SERVICE_ORD');
Insert into VM_APP_DOMAINE (IDENTIFIANT,CODE,LIBELLE,TYPE) values ('CF001','CF001','CF 001','SERVICE_CF');
Insert into VM_APP_DOMAINE (IDENTIFIANT,CODE,LIBELLE,TYPE) values ('CPT001','CPT001','CPT 001','SERVICE_CPT');

Insert into VM_APP_SECTION (IDENTIFIANT,CODE,LIBELLE) values ('327','327','Ministère du Budget et du Portefeuille de l''Etat');

Insert into VM_APP_UNITE_ADMINISTRATIVE (IDENTIFIANT,CODE,LIBELLE,SECTION) values ('13010222','13010222','DTI','327');
Insert into VM_APP_UNITE_ADMINISTRATIVE (IDENTIFIANT,CODE,LIBELLE,SECTION) values ('13010223','13010223','DSI','327');

Insert into VM_APP_USB (IDENTIFIANT,CODE,LIBELLE,SECTION) values ('22086','22086','Budget','327');

Insert into VM_APP_LOCALITE (IDENTIFIANT,CODE,LIBELLE) values ('DIMBOKRO','DIMBOKRO','Dimbokro');

Insert into SERVICE_ORD (IDENTIFIANT,CODE,LIBELLE,USB,LOCALITE) values ('ORD001','ORD001','Budget Dimbokro','22086','DIMBOKRO');
Insert into SERVICE_CF (IDENTIFIANT,CODE,LIBELLE) values ('CF001','CF001','CF 001');
Insert into SERVICE_CPT (IDENTIFIANT,CODE,LIBELLE) values ('CPT001','CPT001','CPT 001');

Insert into TYPE_FONCTION (IDENTIFIANT,CODE,LIBELLE) values ('ADMINISTRATIF','ADMINISTRATIF','Administrative');
Insert into TYPE_FONCTION (IDENTIFIANT,CODE,LIBELLE) values ('BUDGETAIRE','BUDGETAIRE','Budgétaire');

Insert into FONCTION (IDENTIFIANT,CODE,LIBELLE,TYPE,NOMBRE_ACTEUR_PAR_POSTE) values ('ACF','ACF','Assistant contrôleur financier','BUDGETAIRE',null);
Insert into FONCTION (IDENTIFIANT,CODE,LIBELLE,TYPE,NOMBRE_ACTEUR_PAR_POSTE) values ('ACA','ACPT','Assistant comptable assignataire','BUDGETAIRE',null);
Insert into FONCTION (IDENTIFIANT,CODE,LIBELLE,TYPE,NOMBRE_ACTEUR_PAR_POSTE) values ('AGC','AGC','Assistant gestionnaire de credits','BUDGETAIRE',null);
Insert into FONCTION (IDENTIFIANT,CODE,LIBELLE,TYPE,NOMBRE_ACTEUR_PAR_POSTE) values ('AOD','AORD','Assistant ordonnateur','BUDGETAIRE',1);
Insert into FONCTION (IDENTIFIANT,CODE,LIBELLE,TYPE,NOMBRE_ACTEUR_PAR_POSTE) values ('CF','CF','Contrôleur financier','BUDGETAIRE',null);
Insert into FONCTION (IDENTIFIANT,CODE,LIBELLE,TYPE,NOMBRE_ACTEUR_PAR_POSTE) values ('CA','CPT','Comptable Assignataire','BUDGETAIRE',null);
Insert into FONCTION (IDENTIFIANT,CODE,LIBELLE,TYPE,NOMBRE_ACTEUR_PAR_POSTE) values ('GC','GC','Gestionnaire de credits','BUDGETAIRE',null);
Insert into FONCTION (IDENTIFIANT,CODE,LIBELLE,TYPE,NOMBRE_ACTEUR_PAR_POSTE) values ('OD','ORD','Ordonnateur','BUDGETAIRE',1);

Insert into TYPE_DOMAINE_FONCTION (IDENTIFIANT,TYPE_DOMAINE,FONCTION,POSTE_DERIVABLE,POSTE_CODE_SCRIPT,POSTE_LIBELLE_SCRIPT) values ('1','UA','GC',1,'poste.codePrefix+(''00000''+(numero_ordre++)).slice(-5)','poste.fonction.libelle+'' ''+poste.ua.libelle');
Insert into TYPE_DOMAINE_FONCTION (IDENTIFIANT,TYPE_DOMAINE,FONCTION,POSTE_DERIVABLE,POSTE_CODE_SCRIPT,POSTE_LIBELLE_SCRIPT) values ('2','SERVICE_ORD','OD',1,'poste.codePrefix+(''00000''+(numero_ordre++)).slice(-5)','poste.libelle == null ? poste.fonction.libelle+'' ''+(poste.localite == null ? ''délégué'' : ''secondaire'')+'' ''+(poste.usb == null ? ''NULL'' : (poste.usb.code.startsWith("0") || poste.usb.code.startsWith("1") ? ''de la Dotation'' : ''du Programme''))+'' ''+(poste.usb == null ? ''NULL'' : (poste.usb.libelle.toLowerCase().startsWith("programme ") ? poste.usb.libelle.slice(10) : poste.usb.libelle))+(poste.usb.code.startsWith("21") ? '' du ''+poste.section.libelle : '''')+(poste.localite == null ? '''' : '' a ''+(poste.localite.libelle.toLowerCase().startsWith("sous-préfecture de ") ? poste.localite.libelle.slice(19) : poste.localite.libelle)) : poste.libelle');
Insert into TYPE_DOMAINE_FONCTION (IDENTIFIANT,TYPE_DOMAINE,FONCTION,POSTE_DERIVABLE,POSTE_CODE_SCRIPT,POSTE_LIBELLE_SCRIPT) values ('3','SERVICE_CF','CF',1,'poste.codePrefix+(''00000''+(numero_ordre++)).slice(-5)','poste.service_cf.libelle');
Insert into TYPE_DOMAINE_FONCTION (IDENTIFIANT,TYPE_DOMAINE,FONCTION,POSTE_DERIVABLE,POSTE_CODE_SCRIPT,POSTE_LIBELLE_SCRIPT) values ('4','SERVICE_CPT','CA',1,'"''T''+(poste.service_cpt.libelle.toLowerCase().startsWith(''paierie'')?',null);
Insert into TYPE_DOMAINE_FONCTION (IDENTIFIANT,TYPE_DOMAINE,FONCTION,POSTE_DERIVABLE,POSTE_CODE_SCRIPT,POSTE_LIBELLE_SCRIPT) values ('5','UA','AGC',1,'(''A1''+(poste.titulaire == null || poste.titulaire.code == null ? '''' : poste.titulaire.code).slice(-5))+(poste.titulaire.incrementerNombreAssistant()-1)','poste.fonction.libelle+'' ''+poste.ua.libelle');
Insert into TYPE_DOMAINE_FONCTION (IDENTIFIANT,TYPE_DOMAINE,FONCTION,POSTE_DERIVABLE,POSTE_CODE_SCRIPT,POSTE_LIBELLE_SCRIPT) values ('6','SERVICE_ORD','AOD',1,'(''A2''+(poste.titulaire == null || poste.titulaire.code == null ? '''' : poste.titulaire.code).slice(-5))+(poste.titulaire.incrementerNombreAssistant()-1)','poste.libelle == null ? poste.fonction.libelle+'' ''+(poste.localite == null ? ''délégué'' : ''secondaire'')+'' ''+(poste.usb.code.startsWith("0") || poste.usb.code.startsWith("1") ? ''de la Dotation'' : ''du Programme'')+'' ''+(poste.usb.libelle.toLowerCase().startsWith("programme ") ? poste.usb.libelle.slice(10) : poste.usb.libelle)+(poste.usb.code.startsWith("21") ? '' du ''+poste.section.libelle : '''')+(poste.localite == null ? '''' : '' a ''+(poste.localite.libelle.toLowerCase().startsWith("sous-préfecture de ") ? poste.localite.libelle.slice(19) : poste.localite.libelle)) : poste.libelle');
Insert into TYPE_DOMAINE_FONCTION (IDENTIFIANT,TYPE_DOMAINE,FONCTION,POSTE_DERIVABLE,POSTE_CODE_SCRIPT,POSTE_LIBELLE_SCRIPT) values ('7','SERVICE_CF','ACF',1,'(''A3''+(poste.titulaire == null || poste.titulaire.code == null ? '''' : poste.titulaire.code).slice(-5))+(poste.titulaire.incrementerNombreAssistant()-1)','''Assistant ''+poste.service_cf.libelle');
Insert into TYPE_DOMAINE_FONCTION (IDENTIFIANT,TYPE_DOMAINE,FONCTION,POSTE_DERIVABLE,POSTE_CODE_SCRIPT,POSTE_LIBELLE_SCRIPT) values ('8','SERVICE_CPT','ACA',1,'(''A4''+(poste.titulaire == null || poste.titulaire.code == null ? '''' : poste.titulaire.code).slice(-5))+(poste.titulaire.incrementerNombreAssistant()-1)','''Assistant ''+poste.service_cpt.libelle');

Insert into VM_APP_NATURE_DEPENSE (IDENTIFIANT,CODE,LIBELLE) values ('1','1','Personnel');
Insert into VM_APP_NATURE_DEPENSE (IDENTIFIANT,CODE,LIBELLE) values ('2','2','Bienss et services');
Insert into VM_APP_NATURE_DEPENSE (IDENTIFIANT,CODE,LIBELLE) values ('3','3','Transferts');
Insert into VM_APP_NATURE_DEPENSE (IDENTIFIANT,CODE,LIBELLE) values ('4','4','Investissements');

Insert into VM_APP_EX_IMPUTATION (LDEP_ID,IDENTIFIANT,EXERCICE,CODE,LIBELLE,SECTION_IDENTIFIANT,SECTION_CODE,SECTION_CODE_LIBELLE) values ('1','1',2021,'1','1','327','327','327 Budget');

Insert into POSTE (IDENTIFIANT,CODE,LIBELLE,FONCTION,DOMAINE,NUMERO_DOCUMENT) values ('1','1','Assistant','AGC','13010222',1);
Insert into POSTE (IDENTIFIANT,CODE,LIBELLE,FONCTION,DOMAINE,NUMERO_DOCUMENT) values ('C300000','C300000','Gestionnaire','CF','13010223',1);
Insert into POSTE (IDENTIFIANT,CODE,LIBELLE,FONCTION,DOMAINE,NUMERO_DOCUMENT) values ('A3000000','A3000000','Assistant','ACF','13010223',1);

Insert into AFFECTATIONS (IDENTIFIANT,IMPUTATION) values ('1','1');