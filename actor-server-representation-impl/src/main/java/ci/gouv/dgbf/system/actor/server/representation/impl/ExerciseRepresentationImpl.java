package ci.gouv.dgbf.system.actor.server.representation.impl;

import java.io.Serializable;

import javax.enterprise.context.ApplicationScoped;

import ci.gouv.dgbf.system.actor.server.representation.api.ExerciseRepresentation;
import ci.gouv.dgbf.system.actor.server.representation.entities.ExerciseDto;
import org.cyk.utility.server.representation.AbstractRepresentationEntityImpl;

@ApplicationScoped
public class ExerciseRepresentationImpl extends AbstractRepresentationEntityImpl<ExerciseDto> implements ExerciseRepresentation,Serializable {
	private static final long serialVersionUID = 1L;
	
}
