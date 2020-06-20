package ci.gouv.dgbf.system.actor.server.persistence.impl;
import java.io.Serializable;

import javax.enterprise.context.ApplicationScoped;

import ci.gouv.dgbf.system.actor.server.persistence.api.PrivilegeTypePersistence;
import ci.gouv.dgbf.system.actor.server.persistence.entities.PrivilegeType;
import org.cyk.utility.server.persistence.AbstractPersistenceEntityImpl;

@ApplicationScoped
public class PrivilegeTypePersistenceImpl extends AbstractPersistenceEntityImpl<PrivilegeType> implements PrivilegeTypePersistence,Serializable {
	private static final long serialVersionUID = 1L;

	
}