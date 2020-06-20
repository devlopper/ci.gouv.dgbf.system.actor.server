package ci.gouv.dgbf.system.actor.server.business.impl;

import java.io.Serializable;

import javax.enterprise.context.ApplicationScoped;

import ci.gouv.dgbf.system.actor.server.business.api.UserAccountBusiness;
import ci.gouv.dgbf.system.actor.server.persistence.api.UserAccountPersistence;
import ci.gouv.dgbf.system.actor.server.persistence.entities.UserAccount;
import org.cyk.utility.server.business.AbstractBusinessEntityImpl;

@ApplicationScoped
public class UserAccountBusinessImpl extends AbstractBusinessEntityImpl<UserAccount, UserAccountPersistence> implements UserAccountBusiness,Serializable {
	private static final long serialVersionUID = 1L;
		
}
