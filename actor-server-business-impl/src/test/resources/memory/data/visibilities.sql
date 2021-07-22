Insert into TYPE_DOMAINE (IDENTIFIANT,CODE,LIBELLE) values ('SECTION','SECTION','Section');
Insert into TYPE_DOMAINE (IDENTIFIANT,CODE,LIBELLE) values ('UA','UA','Unité administrative');
Insert into TYPE_DOMAINE (IDENTIFIANT,CODE,LIBELLE) values ('USB','USB','Unité de spécialisation du budget');
Insert into TYPE_DOMAINE (IDENTIFIANT,CODE,LIBELLE) values ('ACTION','ACTION','Action');
Insert into TYPE_DOMAINE (IDENTIFIANT,CODE,LIBELLE) values ('ACTIVITE','ACTIVITE','Activité');

Insert into TYPE_DOMAINE (IDENTIFIANT,CODE,LIBELLE) values ('CATEGORIE_ACTIVITE','CATEGORIE_ACTIVITE','Catégorie Activité');
Insert into TYPE_DOMAINE (IDENTIFIANT,CODE,LIBELLE) values ('LOCALITE','LOCALITE','Localité');

Insert into VM_APP_SECTION (IDENTIFIANT,CODE,LIBELLE) values ('s01','s01','Section 01');
Insert into VM_APP_DOMAINE (IDENTIFIANT,CODE,LIBELLE,TYPE) values ('s01','s01','Section 01','SECTION');
Insert into VM_APP_SECTION (IDENTIFIANT,CODE,LIBELLE) values ('s02','s02','Section 02');
Insert into VM_APP_DOMAINE (IDENTIFIANT,CODE,LIBELLE,TYPE) values ('s02','s02','Section 02','SECTION');
Insert into VM_APP_SECTION (IDENTIFIANT,CODE,LIBELLE) values ('s03','s03','Section 03');
Insert into VM_APP_DOMAINE (IDENTIFIANT,CODE,LIBELLE,TYPE) values ('s03','s03','Section 03','SECTION');
Insert into VM_APP_SECTION (IDENTIFIANT,CODE,LIBELLE) values ('s04','s04','Section 04');
Insert into VM_APP_DOMAINE (IDENTIFIANT,CODE,LIBELLE,TYPE) values ('s04','s04','Section 04','SECTION');
Insert into VM_APP_SECTION (IDENTIFIANT,CODE,LIBELLE) values ('s05','s05','Section 05');
Insert into VM_APP_DOMAINE (IDENTIFIANT,CODE,LIBELLE,TYPE) values ('s05','s05','Section 05','SECTION');
Insert into VM_APP_SECTION (IDENTIFIANT,CODE,LIBELLE) values ('s10','s10','Section 10');
Insert into VM_APP_DOMAINE (IDENTIFIANT,CODE,LIBELLE,TYPE) values ('s10','s10','Section 10','SECTION');
Insert into VM_APP_SECTION (IDENTIFIANT,CODE,LIBELLE) values ('s11','s11','Section 11');
Insert into VM_APP_DOMAINE (IDENTIFIANT,CODE,LIBELLE,TYPE) values ('s11','s11','Section 11','SECTION');

Insert into VM_APP_UNITE_ADMINISTRATIVE (IDENTIFIANT,CODE,LIBELLE,SECTION) values ('ua01','ua01','ua 01','s02');
Insert into VM_APP_DOMAINE (IDENTIFIANT,CODE,LIBELLE,TYPE) values ('ua01','ua01','ua 01','UA');
Insert into VM_APP_UNITE_ADMINISTRATIVE (IDENTIFIANT,CODE,LIBELLE,SECTION) values ('ua02','ua02','ua 02','s02');
Insert into VM_APP_DOMAINE (IDENTIFIANT,CODE,LIBELLE,TYPE) values ('ua02','ua02','ua 02','UA');
Insert into VM_APP_UNITE_ADMINISTRATIVE (IDENTIFIANT,CODE,LIBELLE,SECTION) values ('ua10','ua10','ua 10','s11');
Insert into VM_APP_DOMAINE (IDENTIFIANT,CODE,LIBELLE,TYPE) values ('ua10','ua10','ua 10','UA');

Insert into VM_APP_USB (IDENTIFIANT,CODE,LIBELLE,SECTION) values ('usb01','usb01','usb 01','s03');
Insert into VM_APP_DOMAINE (IDENTIFIANT,CODE,LIBELLE,TYPE) values ('usb01','usb01','usb 01','USB');
Insert into VM_APP_USB (IDENTIFIANT,CODE,LIBELLE,SECTION) values ('usb02','usb02','usb 02','s04');
Insert into VM_APP_DOMAINE (IDENTIFIANT,CODE,LIBELLE,TYPE) values ('usb02','usb02','usb 02','USB');
Insert into VM_APP_USB (IDENTIFIANT,CODE,LIBELLE,SECTION) values ('usb03','usb03','usb 03','s05');
Insert into VM_APP_DOMAINE (IDENTIFIANT,CODE,LIBELLE,TYPE) values ('usb03','usb03','usb 03','USB');
Insert into VM_APP_USB (IDENTIFIANT,CODE,LIBELLE,SECTION) values ('usb04','usb04','usb 04','s11');
Insert into VM_APP_DOMAINE (IDENTIFIANT,CODE,LIBELLE,TYPE) values ('usb04','usb04','usb 04','USB');

Insert into VM_APP_ACTION (IDENTIFIANT,CODE,LIBELLE,USB,SECTION) values ('a01','a01','action 01','usb02','s04');
Insert into VM_APP_DOMAINE (IDENTIFIANT,CODE,LIBELLE,TYPE) values ('a01','a01','action 01','ACTION');
Insert into VM_APP_ACTION (IDENTIFIANT,CODE,LIBELLE,USB,SECTION) values ('a02','a02','action 02','usb03','s05');
Insert into VM_APP_DOMAINE (IDENTIFIANT,CODE,LIBELLE,TYPE) values ('a02','a02','action 02','ACTION');
Insert into VM_APP_ACTION (IDENTIFIANT,CODE,LIBELLE,USB,SECTION) values ('a03','a03','action 03','usb04','s11');
Insert into VM_APP_DOMAINE (IDENTIFIANT,CODE,LIBELLE,TYPE) values ('a03','a03','action 03','ACTION');

Insert into VM_APP_ACTIVITE (IDENTIFIANT,CODE,LIBELLE,ACTION,USB,SECTION) values ('ac01','ac01','activite 01','a02','usb03','s05');
Insert into VM_APP_DOMAINE (IDENTIFIANT,CODE,LIBELLE,TYPE) values ('ac01','ac01','activite 01','ACTIVITE');
Insert into VM_APP_ACTIVITE (IDENTIFIANT,CODE,LIBELLE,ACTION,USB,SECTION) values ('ac02','ac02','activite 02','a02','usb03','s05');
Insert into VM_APP_DOMAINE (IDENTIFIANT,CODE,LIBELLE,TYPE) values ('ac02','ac02','activite 02','ACTIVITE');
Insert into VM_APP_ACTIVITE (IDENTIFIANT,CODE,LIBELLE,ACTION,USB,SECTION) values ('ac03','ac03','activite 03','a03','usb04','s11');
Insert into VM_APP_DOMAINE (IDENTIFIANT,CODE,LIBELLE,TYPE) values ('ac03','ac03','activite 03','ACTIVITE');

Insert into ID_IDENTITE (IDENTIFIANT,NOM,PRENOMS,EMAIL) values ('1','komenan','christian','kycdev@gmail.com');
Insert into ACTEUR (IDENTIFIANT,NOM_UTILISATEUR,IDENTITE) values ('1','christian','1');
Insert into ID_IDENTITE (IDENTIFIANT,NOM,PRENOMS,EMAIL) values ('2','rffim_s01','rffim_s01','rffim_s01');
Insert into ACTEUR (IDENTIFIANT,NOM_UTILISATEUR,IDENTITE) values ('2','rffim_s01','2');
Insert into ID_IDENTITE (IDENTIFIANT,NOM,PRENOMS,EMAIL) values ('3','gc_ua02','gc_ua02','gc_ua02');
Insert into ACTEUR (IDENTIFIANT,NOM_UTILISATEUR,IDENTITE) values ('3','gc_ua02','3');

Insert into ACTEUR_DOMAINE (IDENTIFIANT,ACTEUR,DOMAINE) values ('10','1','s01'); -- s01
Insert into ACTEUR_DOMAINE (IDENTIFIANT,ACTEUR,DOMAINE,VISIBLE) values ('11','1','s03',false);
-- Insert into ACTEUR_DOMAINE (IDENTIFIANT,ACTEUR,DOMAINE) values ('11','1','s10'); -- s10
-- Insert into ACTEUR_DOMAINE (IDENTIFIANT,ACTEUR,DOMAINE) values ('12','2','s01');
-- 
-- Insert into ACTEUR_DOMAINE (IDENTIFIANT,ACTEUR,DOMAINE) values ('20','1','ua01'); -- s02
-- Insert into ACTEUR_DOMAINE (IDENTIFIANT,ACTEUR,DOMAINE) values ('21','3','ua02');
-- 
-- Insert into ACTEUR_DOMAINE (IDENTIFIANT,ACTEUR,DOMAINE) values ('30','1','usb01'); -- s03
-- Insert into ACTEUR_DOMAINE (IDENTIFIANT,ACTEUR,DOMAINE) values ('40','1','a01'); -- s04
-- 
-- Insert into ACTEUR_DOMAINE (IDENTIFIANT,ACTEUR,DOMAINE) values ('50','1','ac01'); -- s05
-- Insert into ACTEUR_DOMAINE (IDENTIFIANT,ACTEUR,DOMAINE) values ('51','1','ac02');