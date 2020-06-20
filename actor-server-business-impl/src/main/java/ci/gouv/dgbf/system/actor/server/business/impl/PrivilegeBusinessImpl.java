package ci.gouv.dgbf.system.actor.server.business.impl;

import java.io.Serializable;

import javax.enterprise.context.ApplicationScoped;

import ci.gouv.dgbf.system.actor.server.business.api.PrivilegeBusiness;
import ci.gouv.dgbf.system.actor.server.persistence.api.PrivilegePersistence;
import ci.gouv.dgbf.system.actor.server.persistence.entities.Privilege;
import org.cyk.utility.server.business.AbstractBusinessEntityImpl;

@ApplicationScoped
public class PrivilegeBusinessImpl extends AbstractBusinessEntityImpl<Privilege, PrivilegePersistence> implements PrivilegeBusiness,Serializable {
	private static final long serialVersionUID = 1L;
		
}
