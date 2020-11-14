package ci.gouv.dgbf.system.actor.server.persistence.impl;
import java.io.Serializable;

import javax.enterprise.context.ApplicationScoped;

import ci.gouv.dgbf.system.actor.server.persistence.api.ClusterPrivilegePersistence;
import ci.gouv.dgbf.system.actor.server.persistence.entities.ClusterPrivilege;
import org.cyk.utility.server.persistence.AbstractPersistenceEntityImpl;

@ApplicationScoped
public class ClusterPrivilegePersistenceImpl extends AbstractPersistenceEntityImpl<ClusterPrivilege> implements ClusterPrivilegePersistence,Serializable {
	private static final long serialVersionUID = 1L;

	
}