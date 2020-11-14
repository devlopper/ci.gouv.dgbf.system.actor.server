package ci.gouv.dgbf.system.actor.server.business.impl;

import java.io.Serializable;

import javax.enterprise.context.ApplicationScoped;

import ci.gouv.dgbf.system.actor.server.business.api.ClusterPrivilegeBusiness;
import ci.gouv.dgbf.system.actor.server.persistence.api.ClusterPrivilegePersistence;
import ci.gouv.dgbf.system.actor.server.persistence.entities.ClusterPrivilege;
import org.cyk.utility.server.business.AbstractBusinessEntityImpl;

@ApplicationScoped
public class ClusterPrivilegeBusinessImpl extends AbstractBusinessEntityImpl<ClusterPrivilege, ClusterPrivilegePersistence> implements ClusterPrivilegeBusiness,Serializable {
	private static final long serialVersionUID = 1L;
		
}
