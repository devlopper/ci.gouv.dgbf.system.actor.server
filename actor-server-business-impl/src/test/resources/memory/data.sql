Insert into TYPE_DOMAINE (IDENTIFIANT,CODE,LIBELLE,NUMERO_ORDRE) values ('SECTION','SECTION','Section',1);
Insert into TYPE_DOMAINE (IDENTIFIANT,CODE,LIBELLE,NUMERO_ORDRE) values ('USB','USB','Unité de spécialisation du budget',2);
Insert into TYPE_DOMAINE (IDENTIFIANT,CODE,LIBELLE,NUMERO_ORDRE) values ('CATEGORIE_ACTIVITE','CATEGORIE_ACTIVITE','Catégorie Activité',3);
Insert into TYPE_DOMAINE (IDENTIFIANT,CODE,LIBELLE,NUMERO_ORDRE) values ('ACTIVITE','ACTIVITE','Activité',4);
Insert into TYPE_DOMAINE (IDENTIFIANT,CODE,LIBELLE,NUMERO_ORDRE) values ('UA','UA','Unité administrative',5);
Insert into TYPE_DOMAINE (IDENTIFIANT,CODE,LIBELLE,NUMERO_ORDRE) values ('LOCALITE','LOCALITE','Localité',6);
Insert into TYPE_DOMAINE (IDENTIFIANT,CODE,LIBELLE,NUMERO_ORDRE) values ('SERVICE_ORD','SERVICE_ORD','Services ordonnateurs',7);
Insert into TYPE_DOMAINE (IDENTIFIANT,CODE,LIBELLE,NUMERO_ORDRE) values ('SERVICE_CF','SERVICE_CF','Services contrôleurs financiers',8);
Insert into TYPE_DOMAINE (IDENTIFIANT,CODE,LIBELLE,NUMERO_ORDRE) values ('SERVICE_CPT','SERVICE_CPT','Postes comptables',9);

Insert into VM_APP_DOMAINE (IDENTIFIANT,CODE,LIBELLE,TYPE) values ('13010222','13010222','DTI','UA');

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

Insert into TYPE_DOMAINE_FONCTION (IDENTIFIANT,TYPE_DOMAINE,FONCTION,POSTE_DERIVABLE,POSTE_CODE_SCRIPT,POSTE_LIBELLE_SCRIPT) values ('1','UA','GC',1,'''G1''+(''00000''+(numero_ordre++)).slice(-5)','poste.fonction.libelle+'' ''+poste.ua.libelle');
Insert into TYPE_DOMAINE_FONCTION (IDENTIFIANT,TYPE_DOMAINE,FONCTION,POSTE_DERIVABLE,POSTE_CODE_SCRIPT,POSTE_LIBELLE_SCRIPT) values ('2','UA','AGC',1,'(''A1''+(poste.titulaire == null || poste.titulaire.code == null ? '''' : poste.titulaire.code).slice(-5))+(poste.titulaire.incrementerNombreAssistant()-1)','poste.fonction.libelle+'' ''+poste.ua.libelle');

Insert into VM_APP_NATURE_DEPENSE (IDENTIFIANT,CODE,LIBELLE) values ('1','1','Personnel');
Insert into VM_APP_NATURE_DEPENSE (IDENTIFIANT,CODE,LIBELLE) values ('2','2','Bienss et services');
Insert into VM_APP_NATURE_DEPENSE (IDENTIFIANT,CODE,LIBELLE) values ('3','3','Transferts');
Insert into VM_APP_NATURE_DEPENSE (IDENTIFIANT,CODE,LIBELLE) values ('4','4','Investissements');