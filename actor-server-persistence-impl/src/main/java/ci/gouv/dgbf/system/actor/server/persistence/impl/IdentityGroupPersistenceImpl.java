package ci.gouv.dgbf.system.actor.server.persistence.impl;
import java.io.Serializable;

import javax.enterprise.context.ApplicationScoped;

import ci.gouv.dgbf.system.actor.server.persistence.api.IdentityGroupPersistence;
import ci.gouv.dgbf.system.actor.server.persistence.entities.IdentityGroup;
import org.cyk.utility.server.persistence.AbstractPersistenceEntityImpl;

@ApplicationScoped
public class IdentityGroupPersistenceImpl extends AbstractPersistenceEntityImpl<IdentityGroup> implements IdentityGroupPersistence,Serializable {
	private static final long serialVersionUID = 1L;

	
}