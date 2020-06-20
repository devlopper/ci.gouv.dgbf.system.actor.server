package ci.gouv.dgbf.system.actor.server.business.impl;

import java.io.Serializable;

import javax.enterprise.context.ApplicationScoped;

import ci.gouv.dgbf.system.actor.server.business.api.ProfileTypeBusiness;
import ci.gouv.dgbf.system.actor.server.persistence.api.ProfileTypePersistence;
import ci.gouv.dgbf.system.actor.server.persistence.entities.ProfileType;
import org.cyk.utility.server.business.AbstractBusinessEntityImpl;

@ApplicationScoped
public class ProfileTypeBusinessImpl extends AbstractBusinessEntityImpl<ProfileType, ProfileTypePersistence> implements ProfileTypeBusiness,Serializable {
	private static final long serialVersionUID = 1L;
		
}
