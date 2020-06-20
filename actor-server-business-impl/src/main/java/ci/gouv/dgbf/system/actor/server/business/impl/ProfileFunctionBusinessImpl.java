package ci.gouv.dgbf.system.actor.server.business.impl;

import java.io.Serializable;

import javax.enterprise.context.ApplicationScoped;

import ci.gouv.dgbf.system.actor.server.business.api.ProfileFunctionBusiness;
import ci.gouv.dgbf.system.actor.server.persistence.api.ProfileFunctionPersistence;
import ci.gouv.dgbf.system.actor.server.persistence.entities.ProfileFunction;
import org.cyk.utility.server.business.AbstractBusinessEntityImpl;

@ApplicationScoped
public class ProfileFunctionBusinessImpl extends AbstractBusinessEntityImpl<ProfileFunction, ProfileFunctionPersistence> implements ProfileFunctionBusiness,Serializable {
	private static final long serialVersionUID = 1L;
		
}
