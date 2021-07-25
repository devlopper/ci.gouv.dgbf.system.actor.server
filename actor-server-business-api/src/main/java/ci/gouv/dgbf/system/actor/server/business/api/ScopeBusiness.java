package ci.gouv.dgbf.system.actor.server.business.api;

import java.util.Collection;

import org.cyk.utility.server.business.BusinessEntity;

import ci.gouv.dgbf.system.actor.server.persistence.entities.Scope;

public interface ScopeBusiness extends BusinessEntity<Scope> {

	Collection<Scope> getVisibleSections(String actorCode);
	
	Collection<Scope> getVisibleAdministrativeUnits(String actorCode);
	
	Collection<Scope> getVisibleBudgetSpecializationUnits(String actorCode);
	
	Collection<Scope> getByTypeCodeByActorCode(String typeCode,String actorCode,Boolean visible,Boolean pageable,Integer firstTupleIndex,Integer numberOfTuples);
}