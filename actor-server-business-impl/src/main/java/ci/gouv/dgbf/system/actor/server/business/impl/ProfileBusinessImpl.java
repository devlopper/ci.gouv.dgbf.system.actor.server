package ci.gouv.dgbf.system.actor.server.business.impl;

import java.io.Serializable;

import javax.enterprise.context.ApplicationScoped;

import ci.gouv.dgbf.system.actor.server.business.api.ProfileBusiness;
import ci.gouv.dgbf.system.actor.server.persistence.api.ProfilePersistence;
import ci.gouv.dgbf.system.actor.server.persistence.entities.Profile;
import org.cyk.utility.server.business.AbstractBusinessEntityImpl;

@ApplicationScoped
public class ProfileBusinessImpl extends AbstractBusinessEntityImpl<Profile, ProfilePersistence> implements ProfileBusiness,Serializable {
	private static final long serialVersionUID = 1L;
		
}
