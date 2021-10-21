package ci.gouv.dgbf.system.actor.server.business.api;

import java.util.Collection;

import org.cyk.utility.business.SpecificBusiness;

import ci.gouv.dgbf.system.actor.server.persistence.entities.Scope;

public interface ScopeBusiness extends SpecificBusiness<Scope> {

	Collection<Scope> get(String typeCode,String actorCode,Boolean visible,Boolean pageable,Integer firstTupleIndex,Integer numberOfTuples,Boolean removeTypeCodeFromIdentifier);
	
	Collection<Scope> getByActorCode(String actorCode,String typeCode,Boolean visible,Boolean pageable,Integer firstTupleIndex,Integer numberOfTuples,Boolean removeTypeCodeFromIdentifier);
	
	Collection<Scope> getVisibleSections(String actorCode);
	
	Collection<Scope> getVisibleAdministrativeUnits(String actorCode);
	
	Collection<Scope> getVisibleBudgetSpecializationUnits(String actorCode);
	
	Collection<Scope> getByTypeCodeByActorCode(String typeCode,String actorCode,Boolean visible,Boolean pageable,Integer firstTupleIndex,Integer numberOfTuples,Boolean removeTypeCodeFromIdentifier);
	
	Collection<Scope> getSectionsByActorCode(String actorCode,Boolean visible,Boolean pageable,Integer firstTupleIndex,Integer numberOfTuples,Boolean removeTypeCodeFromIdentifier);
	Collection<String> getSectionsIdentifiersByActorCode(String actorCode,Boolean visible,Boolean pageable,Integer firstTupleIndex,Integer numberOfTuples,Boolean removeTypeCodeFromIdentifier);
	
	Collection<Scope> getAdministrativeUnitsByActorCode(String actorCode,Boolean visible,Boolean pageable,Integer firstTupleIndex,Integer numberOfTuples,Boolean removeTypeCodeFromIdentifier);
	
	Collection<Scope> getBudgetSpecializationUnitsByActorCode(String actorCode,Boolean visible,Boolean pageable,Integer firstTupleIndex,Integer numberOfTuples,Boolean removeTypeCodeFromIdentifier);
}