package ci.gouv.dgbf.system.actor.server.persistence.impl;
import java.io.Serializable;

import javax.enterprise.context.ApplicationScoped;

import ci.gouv.dgbf.system.actor.server.persistence.api.AssignmentsPersistence;
import ci.gouv.dgbf.system.actor.server.persistence.entities.Assignments;
import org.cyk.utility.server.persistence.AbstractPersistenceEntityImpl;

@ApplicationScoped
public class AssignmentsPersistenceImpl extends AbstractPersistenceEntityImpl<Assignments> implements AssignmentsPersistence,Serializable {
	private static final long serialVersionUID = 1L;

	
}