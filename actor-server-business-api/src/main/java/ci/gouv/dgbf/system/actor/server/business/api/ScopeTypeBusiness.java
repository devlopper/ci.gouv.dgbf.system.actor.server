package ci.gouv.dgbf.system.actor.server.business.api;

import java.util.Collection;

import org.cyk.utility.business.SpecificBusiness;

import ci.gouv.dgbf.system.actor.server.persistence.entities.ScopeType;

public interface ScopeTypeBusiness extends SpecificBusiness<ScopeType> {

	Collection<ScopeType> get(Boolean requestable,Boolean pageable,Integer firstTupleIndex,Integer numberOfTuples);
	
}