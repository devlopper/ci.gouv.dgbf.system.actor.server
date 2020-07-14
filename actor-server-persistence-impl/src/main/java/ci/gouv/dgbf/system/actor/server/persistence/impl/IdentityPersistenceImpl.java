package ci.gouv.dgbf.system.actor.server.persistence.impl;
import java.io.Serializable;

import javax.enterprise.context.ApplicationScoped;

import ci.gouv.dgbf.system.actor.server.persistence.api.IdentityPersistence;
import ci.gouv.dgbf.system.actor.server.persistence.entities.Identity;
import org.cyk.utility.server.persistence.AbstractPersistenceEntityImpl;

@ApplicationScoped
public class IdentityPersistenceImpl extends AbstractPersistenceEntityImpl<Identity> implements IdentityPersistence,Serializable {
	private static final long serialVersionUID = 1L;

	
}