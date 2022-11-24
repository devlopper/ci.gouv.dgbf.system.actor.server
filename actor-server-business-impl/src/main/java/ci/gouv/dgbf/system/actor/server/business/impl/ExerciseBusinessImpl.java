package ci.gouv.dgbf.system.actor.server.business.impl;

import java.io.Serializable;

import javax.enterprise.context.ApplicationScoped;

import ci.gouv.dgbf.system.actor.server.business.api.ExerciseBusiness;
import ci.gouv.dgbf.system.actor.server.persistence.api.ExercisePersistence;
import ci.gouv.dgbf.system.actor.server.persistence.entities.Exercise;
import org.cyk.utility.server.business.AbstractBusinessEntityImpl;

@ApplicationScoped
public class ExerciseBusinessImpl extends AbstractBusinessEntityImpl<Exercise, ExercisePersistence> implements ExerciseBusiness,Serializable {
	private static final long serialVersionUID = 1L;
		
}
