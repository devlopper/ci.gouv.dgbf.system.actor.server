package ci.gouv.dgbf.system.actor.server.business.api;

import java.util.Collection;

import org.cyk.utility.business.SpecificBusiness;

import ci.gouv.dgbf.system.actor.server.persistence.entities.ProfileType;

public interface ProfileTypeBusiness extends SpecificBusiness<ProfileType> {

	Collection<ProfileType> get(Boolean pageable,Integer firstTupleIndex,Integer numberOfTuples);
	
}