package ci.gouv.dgbf.system.actor.server.persistence.impl;
import java.io.Serializable;

import javax.enterprise.context.ApplicationScoped;

import ci.gouv.dgbf.system.actor.server.persistence.api.ActionPersistence;
import ci.gouv.dgbf.system.actor.server.persistence.entities.Action;
import org.cyk.utility.server.persistence.AbstractPersistenceEntityImpl;

@ApplicationScoped
public class ActionPersistenceImpl extends AbstractPersistenceEntityImpl<Action> implements ActionPersistence,Serializable {
	private static final long serialVersionUID = 1L;

	
}