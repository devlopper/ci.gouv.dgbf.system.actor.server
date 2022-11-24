package ci.gouv.dgbf.system.actor.server.persistence.impl;
import java.io.Serializable;

import javax.enterprise.context.ApplicationScoped;

import ci.gouv.dgbf.system.actor.server.persistence.api.ExercisePersistence;
import ci.gouv.dgbf.system.actor.server.persistence.entities.Exercise;
import org.cyk.utility.server.persistence.AbstractPersistenceEntityImpl;

@ApplicationScoped
public class ExercisePersistenceImpl extends AbstractPersistenceEntityImpl<Exercise> implements ExercisePersistence,Serializable {
	private static final long serialVersionUID = 1L;

	
}