package ci.gouv.dgbf.system.actor.server.business.impl;

import java.io.Serializable;

import javax.enterprise.context.ApplicationScoped;

import ci.gouv.dgbf.system.actor.server.business.api.UserBusiness;
import ci.gouv.dgbf.system.actor.server.persistence.api.UserPersistence;
import ci.gouv.dgbf.system.actor.server.persistence.entities.User;
import org.cyk.utility.server.business.AbstractBusinessEntityImpl;

@ApplicationScoped
public class UserBusinessImpl extends AbstractBusinessEntityImpl<User, UserPersistence> implements UserBusiness,Serializable {
	private static final long serialVersionUID = 1L;
		
}
