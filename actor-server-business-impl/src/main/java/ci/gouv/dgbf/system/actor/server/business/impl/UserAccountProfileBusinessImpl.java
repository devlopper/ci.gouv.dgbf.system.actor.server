package ci.gouv.dgbf.system.actor.server.business.impl;

import java.io.Serializable;

import javax.enterprise.context.ApplicationScoped;

import ci.gouv.dgbf.system.actor.server.business.api.UserAccountProfileBusiness;
import ci.gouv.dgbf.system.actor.server.persistence.api.UserAccountProfilePersistence;
import ci.gouv.dgbf.system.actor.server.persistence.entities.UserAccountProfile;
import org.cyk.utility.server.business.AbstractBusinessEntityImpl;

@ApplicationScoped
public class UserAccountProfileBusinessImpl extends AbstractBusinessEntityImpl<UserAccountProfile, UserAccountProfilePersistence> implements UserAccountProfileBusiness,Serializable {
	private static final long serialVersionUID = 1L;
		
}
