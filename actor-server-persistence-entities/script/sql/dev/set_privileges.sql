-- Grant privileges to ACTEUR
-- From CA
GRANT SELECT ON SIIBC_CA.gouvernement TO SIIBC_ACTEUR;
GRANT ON COMMIT REFRESH ON SIIBC_CA.gouvernement TO SIIBC_ACTEUR;
GRANT SELECT ON SIIBC_CA.section_budgetaire TO SIIBC_ACTEUR;
GRANT ON COMMIT REFRESH ON SIIBC_CA.section_budgetaire TO SIIBC_ACTEUR;
GRANT SELECT ON SIIBC_CA.unite_administrative TO SIIBC_ACTEUR;
GRANT ON COMMIT REFRESH ON SIIBC_CA.unite_administrative TO SIIBC_ACTEUR;
GRANT SELECT ON SIIBC_CA.groupe_service TO SIIBC_ACTEUR;
GRANT ON COMMIT REFRESH ON SIIBC_CA.groupe_service TO SIIBC_ACTEUR;
GRANT SELECT ON SIIBC_CA.localite TO SIIBC_ACTEUR;
GRANT ON COMMIT REFRESH ON SIIBC_CA.localite TO SIIBC_ACTEUR;
-- From CPP
GRANT SELECT ON SIIBC_CPP.categorie_budget TO SIIBC_ACTEUR;
GRANT ON COMMIT REFRESH ON SIIBC_CPP.categorie_budget TO SIIBC_ACTEUR;
GRANT SELECT ON SIIBC_CPP.usb TO SIIBC_ACTEUR;
GRANT ON COMMIT REFRESH ON SIIBC_CPP.usb TO SIIBC_ACTEUR;
GRANT SELECT ON SIIBC_CPP.action TO SIIBC_ACTEUR;
GRANT ON COMMIT REFRESH ON SIIBC_CPP.action TO SIIBC_ACTEUR;
-- From ADS
GRANT SELECT ON SIIBC_ADS.activites TO SIIBC_ACTEUR;
GRANT ON COMMIT REFRESH ON SIIBC_ADS.activites TO SIIBC_ACTEUR;
GRANT SELECT ON SIIBC_ADS.activite_de_service TO SIIBC_ACTEUR;
GRANT ON COMMIT REFRESH ON SIIBC_ADS.activite_de_service TO SIIBC_ACTEUR;
GRANT SELECT ON SIIBC_ADS.activite_de_recette TO SIIBC_ACTEUR;
GRANT ON COMMIT REFRESH ON SIIBC_ADS.activite_de_recette TO SIIBC_ACTEUR;
GRANT SELECT ON SIIBC_ADS.categorie_activite TO SIIBC_ACTEUR;
GRANT ON COMMIT REFRESH ON SIIBC_ADS.categorie_activite TO SIIBC_ACTEUR;
-- From NEC
GRANT SELECT ON SIIBC_NEC.nature_economique TO SIIBC_ACTEUR;
GRANT ON COMMIT REFRESH ON SIIBC_NEC.nature_economique TO SIIBC_ACTEUR;
GRANT SELECT ON SIIBC_NEC.nature_depense TO SIIBC_ACTEUR;
GRANT ON COMMIT REFRESH ON SIIBC_NEC.nature_depense TO SIIBC_ACTEUR;
GRANT SELECT ON SIIBC_NEC.table_referentiel TO SIIBC_ACTEUR;
GRANT ON COMMIT REFRESH ON SIIBC_NEC.table_referentiel TO SIIBC_ACTEUR;
GRANT SELECT ON SIIBC_NEC.version_referentiel TO SIIBC_ACTEUR;
GRANT ON COMMIT REFRESH ON SIIBC_NEC.version_referentiel TO SIIBC_ACTEUR;
-- From BUDGET
GRANT SELECT ON SIIBC_BUDGET.ligne_depense TO SIIBC_ACTEUR;
GRANT ON COMMIT REFRESH ON SIIBC_BUDGET.ligne_depense TO SIIBC_ACTEUR;
-- From PRT
GRANT SELECT ON SIIBC_PRT.msvc_module TO SIIBC_ACTEUR;
GRANT ON COMMIT REFRESH ON SIIBC_PRT.msvc_module TO SIIBC_ACTEUR;
GRANT SELECT ON SIIBC_PRT.msvc_service TO SIIBC_ACTEUR;
GRANT ON COMMIT REFRESH ON SIIBC_PRT.msvc_service TO SIIBC_ACTEUR;
GRANT SELECT ON SIIBC_PRT.msvc_menu TO SIIBC_ACTEUR;
GRANT ON COMMIT REFRESH ON SIIBC_PRT.msvc_menu TO SIIBC_ACTEUR;
GRANT SELECT ON SIIBC_PRT.msvc_action TO SIIBC_ACTEUR;
GRANT ON COMMIT REFRESH ON SIIBC_PRT.msvc_action TO SIIBC_ACTEUR;
-- From MEA
GRANT SELECT ON SIIBC_MEA.ligne_de_depenses TO SIIBC_ACTEUR;
GRANT ON COMMIT REFRESH ON SIIBC_MEA.ligne_de_depenses TO SIIBC_ACTEUR;
GRANT SELECT ON SIIBC_MEA.fonction_execution TO SIIBC_ACTEUR;
GRANT ON COMMIT REFRESH ON SIIBC_MEA.fonction_execution TO SIIBC_ACTEUR;