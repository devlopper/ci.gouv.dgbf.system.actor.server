package ci.gouv.dgbf.system.actor.server.business.api;

import ci.gouv.dgbf.system.actor.server.persistence.entities.ProfileFunction;
import org.cyk.utility.server.business.BusinessEntity;

public interface ProfileFunctionBusiness extends BusinessEntity<ProfileFunction> {

	String SAVE = "ProfileFunction.save";
	
}
