package ci.gouv.dgbf.system.actor.server.business.impl;

import java.io.Serializable;

import javax.enterprise.context.ApplicationScoped;

import ci.gouv.dgbf.system.actor.server.business.api.PrivilegeTypeBusiness;
import ci.gouv.dgbf.system.actor.server.persistence.api.PrivilegeTypePersistence;
import ci.gouv.dgbf.system.actor.server.persistence.entities.PrivilegeType;
import org.cyk.utility.server.business.AbstractBusinessEntityImpl;

@ApplicationScoped
public class PrivilegeTypeBusinessImpl extends AbstractBusinessEntityImpl<PrivilegeType, PrivilegeTypePersistence> implements PrivilegeTypeBusiness,Serializable {
	private static final long serialVersionUID = 1L;
		
}
