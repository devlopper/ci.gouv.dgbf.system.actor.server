package ci.gouv.dgbf.system.actor.server.persistence.impl;
import java.io.Serializable;

import javax.enterprise.context.ApplicationScoped;

import ci.gouv.dgbf.system.actor.server.persistence.api.ClusterPrivilegesPersistence;
import ci.gouv.dgbf.system.actor.server.persistence.entities.ClusterPrivileges;
import org.cyk.utility.server.persistence.AbstractPersistenceEntityImpl;

@ApplicationScoped
public class ClusterPrivilegesPersistenceImpl extends AbstractPersistenceEntityImpl<ClusterPrivileges> implements ClusterPrivilegesPersistence,Serializable {
	private static final long serialVersionUID = 1L;

	
}