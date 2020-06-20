package ci.gouv.dgbf.system.actor.server.business.impl;

import java.io.Serializable;

import javax.enterprise.context.ApplicationScoped;

import ci.gouv.dgbf.system.actor.server.business.api.ProfilePrivilegeBusiness;
import ci.gouv.dgbf.system.actor.server.persistence.api.ProfilePrivilegePersistence;
import ci.gouv.dgbf.system.actor.server.persistence.entities.ProfilePrivilege;
import org.cyk.utility.server.business.AbstractBusinessEntityImpl;

@ApplicationScoped
public class ProfilePrivilegeBusinessImpl extends AbstractBusinessEntityImpl<ProfilePrivilege, ProfilePrivilegePersistence> implements ProfilePrivilegeBusiness,Serializable {
	private static final long serialVersionUID = 1L;
		
}
