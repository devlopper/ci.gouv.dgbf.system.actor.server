package ci.gouv.dgbf.system.actor.server.persistence.api;

import org.cyk.utility.server.persistence.PersistenceEntity;

import ci.gouv.dgbf.system.actor.server.persistence.entities.AdministrativeUnit;
import ci.gouv.dgbf.system.actor.server.persistence.entities.BudgetSpecializationUnit;
import ci.gouv.dgbf.system.actor.server.persistence.entities.Locality;
import ci.gouv.dgbf.system.actor.server.persistence.entities.ScopeFunction;

public interface ScopeFunctionPersistence extends PersistenceEntity<ScopeFunction> {

	String computeCreditManagerHolderName(String administrativeUnitIdentifier);
	String computeCreditManagerHolderName(AdministrativeUnit administrativeUnit);
	
	String computeAuthorizingOfficerHolderName(String budgetSpecialisationUnitIdentifier,String localityIdentifier);
	String computeAuthorizingOfficerHolderName(BudgetSpecializationUnit budgetSpecialisationUnit,Locality locality);
	
	static Integer computeNumberOfActor(Boolean shared) {
		return FunctionPersistence.computeNumberOfActor(shared);
	}
	
	static Boolean computeShared(Integer numberOfActor) {
		return FunctionPersistence.computeShared(numberOfActor);
	}
	
	static String computeSharedAsString(Boolean shared) {
		return FunctionPersistence.computeSharedAsString(shared);
	}
}