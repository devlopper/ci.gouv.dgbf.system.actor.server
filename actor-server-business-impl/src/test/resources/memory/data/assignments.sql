INSERT INTO TYPE_FONCTION (IDENTIFIANT,CODE,LIBELLE) VALUES ('BUDGETAIRE','BUDGETAIRE','Budgétaire');
INSERT INTO FONCTION (IDENTIFIANT,CODE,LIBELLE,TYPE) VALUES ('GC','GC','Gestionnaire de crédits','BUDGETAIRE');
INSERT INTO FONCTION (IDENTIFIANT,CODE,LIBELLE,TYPE) VALUES ('AGC','AGC','Assistant gestionnaire de crédits','BUDGETAIRE');
INSERT INTO FONCTION (IDENTIFIANT,CODE,LIBELLE,TYPE) VALUES ('ORD','ORD','Ordonnateur','BUDGETAIRE');
INSERT INTO FONCTION (IDENTIFIANT,CODE,LIBELLE,TYPE) VALUES ('AORD','AORD','Assistant ordonnateur','BUDGETAIRE');
INSERT INTO FONCTION (IDENTIFIANT,CODE,LIBELLE,TYPE) VALUES ('CF','CF','CF','BUDGETAIRE');
INSERT INTO FONCTION (IDENTIFIANT,CODE,LIBELLE,TYPE) VALUES ('ACF','ACF','Assistant cf','BUDGETAIRE');
INSERT INTO FONCTION (IDENTIFIANT,CODE,LIBELLE,TYPE) VALUES ('CPT','CPT','CPT','BUDGETAIRE');
INSERT INTO FONCTION (IDENTIFIANT,CODE,LIBELLE,TYPE) VALUES ('ACPT','ACPT','Assistant cpt','BUDGETAIRE');

INSERT INTO TYPE_DOMAINE (IDENTIFIANT,CODE,LIBELLE) VALUES ('SECTION','SECTION','Section');
INSERT INTO TYPE_DOMAINE (IDENTIFIANT,CODE,LIBELLE) VALUES ('UA','UA','Unité administrative');
INSERT INTO TYPE_DOMAINE (IDENTIFIANT,CODE,LIBELLE) VALUES ('USB','USB','Budget');
INSERT INTO TYPE_DOMAINE (IDENTIFIANT,CODE,LIBELLE) VALUES ('SERVICE_ORD','SERVICE_ORD','Service ordonnateur');
INSERT INTO TYPE_DOMAINE (IDENTIFIANT,CODE,LIBELLE) VALUES ('SERVICE_CF','SERVICE_CF','Service cf');
INSERT INTO TYPE_DOMAINE_FONCTION (IDENTIFIANT,TYPE_DOMAINE,FONCTION,POSTE_DERIVABLE,POSTE_CODE_SCRIPT,POSTE_LIBELLE_SCRIPT) VALUES ('1','UA','GC',1,'poste.codePrefix+(''00000''+(numero_ordre++)).slice(-5)','poste.fonction.libelle+'' ''+poste.ua.libelle');
INSERT INTO TYPE_DOMAINE_FONCTION (IDENTIFIANT,TYPE_DOMAINE,FONCTION,POSTE_DERIVABLE,POSTE_CODE_SCRIPT,POSTE_LIBELLE_SCRIPT) VALUES ('2','UA','AGC',1,'("A1"+(poste.titulaire == null || poste.titulaire.code == null ? "" : poste.titulaire.code).slice(-5))+(poste.titulaire.incrementerNombreAssistant()-1)','poste.libelle == null ? poste.fonction.libelle+" "+poste.ua.libelle : poste.libelle');
INSERT INTO TYPE_DOMAINE_FONCTION (IDENTIFIANT,TYPE_DOMAINE,FONCTION,POSTE_DERIVABLE,POSTE_CODE_SCRIPT,POSTE_LIBELLE_SCRIPT) VALUES ('3','USB','ORD',1,'poste.codePrefix+(''00000''+(numero_ordre++)).slice(-5)','poste.libelle == null ? poste.fonction.libelle+" "+(poste.localite == null ? "délégué" : "secondaire")+" "+(poste.usb.code.startsWith("0") || poste.usb.code.startsWith("1") ? "de la Dotation" : "du Programme")+" "+(poste.usb.libelle.toLowerCase().startsWith("programme ") ? poste.usb.libelle.slice(10) : poste.usb.libelle)+(poste.usb.code.startsWith("21") ? " du "+poste.section.libelle : "")+(poste.localite == null ? "" : " à "+(poste.localite.libelle.toLowerCase().startsWith("sous-préfecture de ") ? poste.localite.libelle.slice(19) : poste.localite.libelle)) : poste.libelle');
INSERT INTO TYPE_DOMAINE_FONCTION (IDENTIFIANT,TYPE_DOMAINE,FONCTION,POSTE_DERIVABLE,POSTE_CODE_SCRIPT,POSTE_LIBELLE_SCRIPT) VALUES ('4','USB','AORD',1,'("A2"+(poste.titulaire == null || poste.titulaire.code == null ? "" : poste.titulaire.code).slice(-5))+(poste.titulaire.incrementerNombreAssistant()-1)','poste.libelle == null ? poste.fonction.libelle+" "+(poste.localite == null ? "délégué" : "secondaire")+" "+(poste.usb.code.startsWith("0") || poste.usb.code.startsWith("1") ? "de la Dotation" : "du Programme")+" "+(poste.usb.libelle.toLowerCase().startsWith("programme ") ? poste.usb.libelle.slice(10) : poste.usb.libelle)+(poste.usb.code.startsWith("21") ? " du "+poste.section.libelle : "")+(poste.localite == null ? "" : " à "+(poste.localite.libelle.toLowerCase().startsWith("sous-préfecture de ") ? poste.localite.libelle.slice(19) : poste.localite.libelle)) : poste.libelle');
INSERT INTO TYPE_DOMAINE_FONCTION (IDENTIFIANT,TYPE_DOMAINE,FONCTION,POSTE_DERIVABLE,POSTE_CODE_SCRIPT,POSTE_LIBELLE_SCRIPT) VALUES ('5','UA','CF',1,'poste.codePrefix+(''00000''+(numero_ordre++)).slice(-5)','poste.libelle == null ? poste.fonction.libelle+'' ''+poste.ua.libelle : poste.libelle');
INSERT INTO TYPE_DOMAINE_FONCTION (IDENTIFIANT,TYPE_DOMAINE,FONCTION,POSTE_DERIVABLE,POSTE_CODE_SCRIPT,POSTE_LIBELLE_SCRIPT) VALUES ('6','UA','ACF',1,'("A3"+(poste.titulaire == null || poste.titulaire.code == null ? "" : poste.titulaire.code).slice(-5))+(poste.titulaire.incrementerNombreAssistant()-1)','poste.libelle == null ? poste.fonction.libelle+'' ''+poste.ua.libelle : poste.libelle');
INSERT INTO TYPE_DOMAINE_FONCTION (IDENTIFIANT,TYPE_DOMAINE,FONCTION,POSTE_DERIVABLE,POSTE_CODE_SCRIPT,POSTE_LIBELLE_SCRIPT) VALUES ('7','UA','CPT',1,'poste.codePrefix+(''00000''+(numero_ordre++)).slice(-5)','poste.libelle == null ? poste.fonction.libelle+'' ''+poste.ua.libelle : poste.libelle');
INSERT INTO TYPE_DOMAINE_FONCTION (IDENTIFIANT,TYPE_DOMAINE,FONCTION,POSTE_DERIVABLE,POSTE_CODE_SCRIPT,POSTE_LIBELLE_SCRIPT) VALUES ('8','UA','ACPT',1,'("A4"+(poste.titulaire == null || poste.titulaire.code == null ? "" : poste.titulaire.code).slice(-5))+(poste.titulaire.incrementerNombreAssistant()-1)','poste.libelle == null ? poste.fonction.libelle+'' ''+poste.ua.libelle : poste.libelle');

INSERT INTO VM_APP_DOMAINE (IDENTIFIANT,CODE,LIBELLE,TYPE) VALUES ('327','327','BUDGET','SECTION');
INSERT INTO VM_APP_DOMAINE (IDENTIFIANT,CODE,LIBELLE,TYPE) VALUES ('DTI','13010222','DTI','UA');
INSERT INTO VM_APP_DOMAINE (IDENTIFIANT,CODE,LIBELLE,TYPE) VALUES ('DBE','13010223','DBE','UA');
INSERT INTO VM_APP_DOMAINE (IDENTIFIANT,CODE,LIBELLE,TYPE) VALUES ('DOCD','13010224','DOCD','UA');
INSERT INTO VM_APP_DOMAINE (IDENTIFIANT,CODE,LIBELLE,TYPE) VALUES ('EPN','32010001','EPN','UA');
INSERT INTO VM_APP_DOMAINE (IDENTIFIANT,CODE,LIBELLE,TYPE) VALUES ('22086','22086','BUDGET','USB');
INSERT INTO VM_APP_DOMAINE (IDENTIFIANT,CODE,LIBELLE,TYPE) VALUES ('1','1','1','USB');
INSERT INTO VM_APP_DOMAINE (IDENTIFIANT,CODE,LIBELLE,TYPE) VALUES ('2','2','2','SERVICE_CF');

INSERT INTO VM_APP_SECTION (IDENTIFIANT,CODE,LIBELLE) VALUES ('327','327','BUDGET');
INSERT INTO VM_APP_UNITE_ADMINISTRATIVE (IDENTIFIANT,CODE,LIBELLE,SECTION) VALUES ('DTI','13010222','DTI','327');
INSERT INTO VM_APP_UNITE_ADMINISTRATIVE (IDENTIFIANT,CODE,LIBELLE,SECTION) VALUES ('DBE','13010223','DBE','327');
INSERT INTO VM_APP_USB (IDENTIFIANT,CODE,LIBELLE,SECTION) VALUES ('22086','22086','22086','327');
INSERT INTO SERVICE_ORD (IDENTIFIANT,CODE,LIBELLE,USB) VALUES ('1','1','1','22086');
INSERT INTO SERVICE_CF (IDENTIFIANT,CODE,LIBELLE,SECTION) VALUES ('2','2','2','327');

INSERT INTO POSTE (IDENTIFIANT,CODE,LIBELLE,FONCTION,DOMAINE,PARENT,NUMERO_DOCUMENT) VALUES ('GCDTI','GCDTI','GCDTI','GC','DTI',null,1);
INSERT INTO POSTE (IDENTIFIANT,CODE,LIBELLE,FONCTION,DOMAINE,PARENT,NUMERO_DOCUMENT) VALUES ('GCDTI0','GCDTI0','GCDTI0','AGC','DTI',null,1);
INSERT INTO POSTE (IDENTIFIANT,CODE,LIBELLE,FONCTION,DOMAINE,PARENT,NUMERO_DOCUMENT) VALUES ('ORDBUDGET','ORDBUDGET','ORDBUDGET','ORD','1',null,1);
INSERT INTO POSTE (IDENTIFIANT,CODE,LIBELLE,FONCTION,DOMAINE,PARENT,NUMERO_DOCUMENT) VALUES ('ORDBUDGET0','ORDBUDGET0','ORDBUDGET0','AORD','1',null,1);
INSERT INTO POSTE (IDENTIFIANT,CODE,LIBELLE,FONCTION,DOMAINE,PARENT,NUMERO_DOCUMENT) VALUES ('CF327','CFBUDGET','CFBUDGET','CF','2',null,1);
INSERT INTO POSTE (IDENTIFIANT,CODE,LIBELLE,FONCTION,DOMAINE,PARENT,NUMERO_DOCUMENT) VALUES ('CF3270','CFBUDGET0','CFBUDGET0','ACF','2',null,1);

INSERT INTO VM_APP_EX_IMPUTATION (IDENTIFIANT,CODE,LIBELLE,LDEP_ID,SECTION_CODE,UA_CODE,USB_CODE) VALUES ('1','1','1','a','327','13010222','22086');
INSERT INTO VM_APP_EX_IMPUTATION (IDENTIFIANT,CODE,LIBELLE,LDEP_ID,SECTION_CODE,UA_CODE,USB_CODE) VALUES ('2','2','1','b','327','13010222','22086');
INSERT INTO VM_APP_EX_IMPUTATION (IDENTIFIANT,CODE,LIBELLE,LDEP_ID,SECTION_CODE,UA_CODE,USB_CODE) VALUES ('3','3','1','c','327','13010222','22086');

INSERT INTO AFFECTATIONS (IDENTIFIANT,IMPUTATION,GC,ORD,CF,CPT) VALUES ('1','1',null,null,null,null);
INSERT INTO AFFECTATIONS (IDENTIFIANT,IMPUTATION,GC,ORD,CF,CPT) VALUES ('2','2',null,null,null,null);
INSERT INTO AFFECTATIONS (IDENTIFIANT,IMPUTATION,GC,ORD,CF,CPT) VALUES ('3','3',null,null,null,null);