package ci.gouv.dgbf.system.actor.server.business.impl;

import java.io.Serializable;

import javax.enterprise.context.ApplicationScoped;

import ci.gouv.dgbf.system.actor.server.business.api.ClusterPrivilegesBusiness;
import ci.gouv.dgbf.system.actor.server.persistence.api.ClusterPrivilegesPersistence;
import ci.gouv.dgbf.system.actor.server.persistence.entities.ClusterPrivileges;
import org.cyk.utility.server.business.AbstractBusinessEntityImpl;

@ApplicationScoped
public class ClusterPrivilegesBusinessImpl extends AbstractBusinessEntityImpl<ClusterPrivileges, ClusterPrivilegesPersistence> implements ClusterPrivilegesBusiness,Serializable {
	private static final long serialVersionUID = 1L;
		
}
