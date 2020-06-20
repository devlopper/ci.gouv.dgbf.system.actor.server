package ci.gouv.dgbf.system.actor.server.persistence.impl;
import java.io.Serializable;

import javax.enterprise.context.ApplicationScoped;

import ci.gouv.dgbf.system.actor.server.persistence.api.PrivilegePersistence;
import ci.gouv.dgbf.system.actor.server.persistence.entities.Privilege;
import org.cyk.utility.server.persistence.AbstractPersistenceEntityImpl;

@ApplicationScoped
public class PrivilegePersistenceImpl extends AbstractPersistenceEntityImpl<Privilege> implements PrivilegePersistence,Serializable {
	private static final long serialVersionUID = 1L;

	
}